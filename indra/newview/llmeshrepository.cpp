/** 
 * @file llmeshrepository.cpp
 * @brief Mesh repository implementation.
 *
 * $LicenseInfo:firstyear=2005&license=viewerlgpl$
 * Second Life Viewer Source Code
 * Copyright (C) 2010, Linden Research, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation;
 * version 2.1 of the License only.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Linden Research, Inc., 945 Battery Street, San Francisco, CA  94111  USA
 * $/LicenseInfo$
 */

#include "llviewerprecompiledheaders.h"

#include "apr_pools.h"
#include "apr_dso.h"
#include "llhttpstatuscodes.h"
#include "llmeshrepository.h"

#include "llagent.h"
#include "llappviewer.h"
#include "llbufferstream.h"
#include "llcallbacklist.h"
#include "lldatapacker.h"
#include "llfasttimer.h"
#include "llfloaterperms.h"
#include "lleconomy.h"
#include "llimagej2c.h"
#include "llhost.h"
#include "llnotificationsutil.h"
#include "llsd.h"
#include "llsdutil_math.h"
#include "llsdserialize.h"
#include "llthread.h"
#include "llvfile.h"
#include "llviewercontrol.h"
#include "llviewerinventory.h"
#include "llviewermenufile.h"
#include "llviewerobjectlist.h"
#include "llviewerregion.h"
#include "llviewertexturelist.h"
#include "llvolume.h"
#include "llvolumemgr.h"
#include "llvovolume.h"
#include "llworld.h"
#include "material_codes.h"
#include "pipeline.h"
#include "llinventorymodel.h"
#include "llfoldertype.h"
#include "llviewerparcelmgr.h"
#include "llassetuploadresponders.h"
#include "lluploadfloaterobservers.h"
#include "aicurl.h"

#include "boost/lexical_cast.hpp"

#ifndef LL_WINDOWS
#include "netdb.h"
#endif

#include <queue>

class AIHTTPTimeoutPolicy;
extern AIHTTPTimeoutPolicy meshHeaderResponder_timeout;
extern AIHTTPTimeoutPolicy meshLODResponder_timeout;
extern AIHTTPTimeoutPolicy meshSkinInfoResponder_timeout;
extern AIHTTPTimeoutPolicy meshDecompositionResponder_timeout;
extern AIHTTPTimeoutPolicy meshPhysicsShapeResponder_timeout;
extern AIHTTPTimeoutPolicy wholeModelFeeResponder_timeout;
extern AIHTTPTimeoutPolicy wholeModelUploadResponder_timeout;

LLMeshRepository gMeshRepo;

const U32 MAX_MESH_REQUESTS_PER_SECOND = 100;

// Maximum mesh version to support.  Three least significant digits are reserved for the minor version, 
// with major version changes indicating a format change that is not backwards compatible and should not
// be parsed by viewers that don't specifically support that version. For example, if the integer "1" is 
// present, the version is 0.001. A viewer that can parse version 0.001 can also parse versions up to 0.999, 
// but not 1.0 (integer 1000).
// See wiki at https://wiki.secondlife.com/wiki/Mesh/Mesh_Asset_Format
const S32 MAX_MESH_VERSION = 999;

U32 LLMeshRepository::sBytesReceived = 0; // Main thread (http responders).
U32 LLMeshRepository::sHTTPRequestCount = 0; // Mesh thread
U32 LLMeshRepository::sHTTPRetryCount = 0; // Main thread (http responders).
std::atomic<U32> LLMeshRepository::sLODProcessing = 0; // Mesh & Main thread.
U32 LLMeshRepository::sLODPending = 0;

U32 LLMeshRepository::sCacheBytesRead = 0;
U32 LLMeshRepository::sCacheBytesWritten = 0;
U32 LLMeshRepository::sPeakKbps = 0;

const U32 MAX_TEXTURE_UPLOAD_RETRIES = 5;

static S32 dump_num = 0;
std::string make_dump_name(std::string prefix, S32 num)
{
	return prefix + boost::lexical_cast<std::string>(num) + std::string(".xml");
	
}
void dump_llsd_to_file(const LLSD& content, std::string filename);
LLSD llsd_from_file(std::string filename);

std::string header_lod[] = 
{
	"lowest_lod",
	"low_lod",
	"medium_lod",
	"high_lod"
};
const char * const LOG_MESH = "Mesh";


//get the number of bytes resident in memory for given volume
U32 get_volume_memory_size(const LLVolume* volume)
{
	U32 indices = 0;
	U32 vertices = 0;

	for (U32 i = 0; i < (U32)volume->getNumVolumeFaces(); ++i)
	{
		const LLVolumeFace& face = volume->getVolumeFace(i);
		indices += face.mNumIndices;
		vertices += face.mNumVertices;
	}


	return indices * 2 + vertices * 11 + sizeof(LLVolume) + sizeof(LLVolumeFace) * volume->getNumVolumeFaces();
}

void get_vertex_buffer_from_mesh(LLCDMeshData& mesh, LLModel::PhysicsMesh& res, F32 scale = 1.f)
{
	res.mPositions.clear();
	res.mNormals.clear();
	
	const F32* v = mesh.mVertexBase;

	if (mesh.mIndexType == LLCDMeshData::INT_16)
	{
		U16* idx = (U16*) mesh.mIndexBase;
		for (S32 j = 0; j < mesh.mNumTriangles; ++j)
		{ 
			F32* mp0 = (F32*) ((U8*)v+idx[0]*mesh.mVertexStrideBytes);
			F32* mp1 = (F32*) ((U8*)v+idx[1]*mesh.mVertexStrideBytes);
			F32* mp2 = (F32*) ((U8*)v+idx[2]*mesh.mVertexStrideBytes);

			idx = (U16*) (((U8*)idx)+mesh.mIndexStrideBytes);
			
			LLVector3 v0(mp0);
			LLVector3 v1(mp1);
			LLVector3 v2(mp2);

			LLVector3 n = (v1-v0)%(v2-v0);
			n.normalize();

			res.mPositions.push_back(v0*scale);
			res.mPositions.push_back(v1*scale);
			res.mPositions.push_back(v2*scale);

			res.mNormals.push_back(n);
			res.mNormals.push_back(n);
			res.mNormals.push_back(n);			
		}
	}
	else
	{
		U32* idx = (U32*) mesh.mIndexBase;
		for (S32 j = 0; j < mesh.mNumTriangles; ++j)
		{ 
			F32* mp0 = (F32*) ((U8*)v+idx[0]*mesh.mVertexStrideBytes);
			F32* mp1 = (F32*) ((U8*)v+idx[1]*mesh.mVertexStrideBytes);
			F32* mp2 = (F32*) ((U8*)v+idx[2]*mesh.mVertexStrideBytes);

			idx = (U32*) (((U8*)idx)+mesh.mIndexStrideBytes);
			
			LLVector3 v0(mp0);
			LLVector3 v1(mp1);
			LLVector3 v2(mp2);

			LLVector3 n = (v1-v0)%(v2-v0);
			n.normalize();

			res.mPositions.push_back(v0*scale);
			res.mPositions.push_back(v1*scale);
			res.mPositions.push_back(v2*scale);

			res.mNormals.push_back(n);
			res.mNormals.push_back(n);
			res.mNormals.push_back(n);			
		}
	}
}

LLViewerFetchedTexture* LLMeshUploadThread::FindViewerTexture(const LLImportMaterial& material)
{
	LLPointer< LLViewerFetchedTexture > * ppTex = static_cast< LLPointer< LLViewerFetchedTexture > * >(material.mOpaqueData);
	return ppTex ? (*ppTex).get() : NULL;
}

std::atomic<U32> LLMeshRepoThread::sActiveHeaderRequests = 0;
std::atomic<U32> LLMeshRepoThread::sActiveLODRequests = 0;
std::atomic<U32> LLMeshRepoThread::sActiveRequests = 0;
std::atomic<U32> LLMeshRepoThread::sActiveSubdataRequests = 0;
U32	LLMeshRepoThread::sMaxConcurrentRequests = 1;

class LLMeshHeaderResponder : public LLHTTPClient::ResponderWithCompleted
{
public:
	LLVolumeParams mMeshParams;
	bool mProcessed;
	void retry();

	LLMeshHeaderResponder(const LLVolumeParams& mesh_params)
		: mMeshParams(mesh_params)
	{
		++LLMeshRepoThread::sActiveHeaderRequests;
		++LLMeshRepoThread::sActiveRequests;
		mProcessed = false;
	}

	~LLMeshHeaderResponder()
	{
		if (!LLApp::isQuitting())
		{
			if (!mProcessed)
			{ //something went wrong, retry
				LL_WARNS() << "Timeout or service unavailable, retrying." << LL_ENDL;
				retry();
			}

			--LLMeshRepoThread::sActiveHeaderRequests;
			--LLMeshRepoThread::sActiveRequests;
		}
	}


	virtual void completedRaw(LLChannelDescriptors const& channels,
							  LLIOPipe::buffer_ptr_t const& buffer);

	/*virtual*/ AICapabilityType capability_type(void) const { return cap_mesh; }
	/*virtual*/ AIHTTPTimeoutPolicy const& getHTTPTimeoutPolicy(void) const { return meshHeaderResponder_timeout; }
	/*virtual*/ char const* getName(void) const { return "LLMeshHeaderResponder"; }
};

class LLMeshResponder : public LLHTTPClient::ResponderWithCompleted
{
public:
	std::function<bool(const std::vector<U8>&)> mHandler;
	std::function<void()> mRetry;
	const std::string mType;
	const LLUUID mMeshID;
	const U32 mOffset;
	const U32 mRequestedBytes;
	bool mProcessed;
	std::atomic<U32>* mCounter;
	LLFrameTimer mTimer;

	LLMeshResponder(std::string type, 
		std::function<bool(const std::vector<U8>&)> handler, 
		std::function<void()> retry,
		const LLUUID& mesh_id, 
		const LLMeshRepoThread::MeshHeaderInfo& header_info,
		std::atomic<U32>* counter = nullptr)
		: mType(type), mHandler(handler), mMeshID(mesh_id), mOffset(header_info.mOffset), mRequestedBytes(header_info.mSize), mProcessed(false), mCounter(counter)
	{
		if (mCounter)
			(*mCounter)++;
		LLMeshRepoThread::sActiveRequests++;
	}

	~LLMeshResponder()
	{
		if (!LLApp::isExiting())
		{
			F32 seconds = mTimer.getElapsedTimeF32();
			if (seconds > 1.f)
			{
				LL_WARNS() << "Mesh " << mMeshID << " " << mType << " fetch took " << seconds << " seconds." << LL_ENDL;
			}
			if (!mProcessed)
			{
				LL_WARNS() << "Killed " << mType << " fetch without being processed, retrying." << LL_ENDL;
				retry();
			}
			if (mCounter)
				(*mCounter)--;
			LLMeshRepoThread::sActiveRequests--;
		}
	}

	void retry();

	virtual void completedRaw(LLChannelDescriptors const& channels,
							  LLIOPipe::buffer_ptr_t const& buffer);

	/*virtual*/ AICapabilityType capability_type(void) const { return cap_mesh; }
	/*virtual*/ AIHTTPTimeoutPolicy const& getHTTPTimeoutPolicy(void) const { return meshLODResponder_timeout; }
	/*virtual*/ char const* getName(void) const { return mType.c_str(); }
};

void log_upload_error(S32 status, const LLSD& content, std::string stage, std::string model_name)
{
	// Add notification popup.
	LLSD args;
	std::string message = content["error"]["message"];
	std::string identifier = content["error"]["identifier"];
	args["MESSAGE"] = message;
	args["IDENTIFIER"] = identifier;
	args["LABEL"] = model_name;
	gMeshRepo.uploadError(args);

	// Log details.
	LL_WARNS() << "stage: " << stage << " http status: " << status << LL_ENDL;
	if (content.has("error"))
	{
		const LLSD& err = content["error"];
		LL_WARNS() << "err: " << err << LL_ENDL;
		LL_WARNS() << "mesh upload failed, stage '" << stage
				<< "' error '" << err["error"].asString()
				<< "', message '" << err["message"].asString()
				<< "', id '" << err["identifier"].asString()
				<< "'" << LL_ENDL;
		if (err.has("errors"))
		{
			S32 error_num = 0;
			const LLSD& err_list = err["errors"];
			for (LLSD::array_const_iterator it = err_list.beginArray();
				 it != err_list.endArray();
				 ++it)
			{
				const LLSD& err_entry = *it;
				LL_WARNS() << "error[" << error_num << "]:" << LL_ENDL;
				for (LLSD::map_const_iterator map_it = err_entry.beginMap();
					 map_it != err_entry.endMap();
					 ++map_it)
				{
					LL_WARNS() << "\t" << map_it->first << ": "
							<< map_it->second << LL_ENDL;
				}
				error_num++;
			}
		}
	}
	else
	{
		LL_WARNS() << "bad mesh, no error information available" << LL_ENDL;
	}
}

class LLWholeModelFeeResponder : public LLHTTPClient::ResponderWithCompleted
{
	std::string& mWholeModelUploadURL;
	LLSD mModelData;
	LLHandle<LLWholeModelFeeObserver> mObserverHandle;
public:
	LLWholeModelFeeResponder(LLSD& model_data, LLHandle<LLWholeModelFeeObserver> observer_handle, std::string& url_out):
		mWholeModelUploadURL(url_out),
		mModelData(model_data),
		mObserverHandle(observer_handle)
	{
	}

	~LLWholeModelFeeResponder()
	{
	}

	virtual void httpCompleted(void)
	{
		LLSD cc = mContent;
		if (gSavedSettings.getS32("MeshUploadFakeErrors")&1)
		{
			cc = llsd_from_file("fake_upload_error.xml");
		}
			
		dump_llsd_to_file(cc,make_dump_name("whole_model_fee_response_",dump_num));

		LLWholeModelFeeObserver* observer = mObserverHandle.get();

		if (isGoodStatus(mStatus) &&
			cc["state"].asString() == "upload")
		{
			mWholeModelUploadURL = cc["uploader"].asString();

			if (observer)
			{
				cc["data"]["upload_price"] = cc["upload_price"];
				observer->onModelPhysicsFeeReceived(cc["data"], mWholeModelUploadURL);
			}
		}
		else
		{
			LL_WARNS() << "fee request failed" << LL_ENDL;
			log_upload_error(mStatus,cc,"fee",mModelData["name"]);
			mWholeModelUploadURL = "";

			if (observer)
			{
				observer->setModelPhysicsFeeErrorStatus(mStatus, mReason);
			}
		}
	}

	/*virtual*/ AIHTTPTimeoutPolicy const& getHTTPTimeoutPolicy(void) const { return wholeModelFeeResponder_timeout; }
	/*virtual*/ char const* getName(void) const { return "LLWholeModelFeeResponder"; }
};

class LLWholeModelUploadResponder : public LLHTTPClient::ResponderWithCompleted
{
	LLSD mModelData;
	LLHandle<LLWholeModelUploadObserver> mObserverHandle;
	
public:
	LLWholeModelUploadResponder(LLSD& model_data, LLHandle<LLWholeModelUploadObserver> observer_handle):
		mModelData(model_data),
		mObserverHandle(observer_handle)
	{
	}

	~LLWholeModelUploadResponder()
	{
	}

	virtual void httpCompleted(void)
	{
		LLSD cc = mContent;
		if (gSavedSettings.getS32("MeshUploadFakeErrors")&2)
		{
			cc = llsd_from_file("fake_upload_error.xml");
		}

		dump_llsd_to_file(cc,make_dump_name("whole_model_upload_response_",dump_num));
		
		LLWholeModelUploadObserver* observer = mObserverHandle.get();

		// requested "mesh" asset type isn't actually the type
		// of the resultant object, fix it up here.
		if (isGoodStatus(mStatus) &&
			cc["state"].asString() == "complete")
		{
			mModelData["asset_type"] = "object";
			gMeshRepo.updateInventory(LLMeshRepository::inventory_data(mModelData,cc));

			if (observer)
			{
				doOnIdleOneTime(boost::bind(&LLWholeModelUploadObserver::onModelUploadSuccess, observer));
			}
		}
		else
		{
			LL_WARNS() << "upload failed" << LL_ENDL;
			std::string model_name = mModelData["name"].asString();
			log_upload_error(mStatus,cc,"upload",model_name);

			if (observer)
			{
				doOnIdleOneTime(boost::bind(&LLWholeModelUploadObserver::onModelUploadFailure, observer));
			}
		}
	}

	/*virtual*/ AIHTTPTimeoutPolicy const& getHTTPTimeoutPolicy(void) const { return wholeModelUploadResponder_timeout; }
	/*virtual*/ char const* getName(void) const { return "LLWholeModelUploadResponder"; }
};

LLMeshRepoThread::LLMeshRepoThread()
	: LLThread("mesh repo"),
	mMutex(new LLMutex()),
	mHeaderMutex(new LLMutex()),
	mSignal(new LLCondition()),
	mSkinRequestsMutex(new LLMutex()),
	mResultQueuesMutex(new LLMutex()),
	mDecompositionRequestsMutex(new LLMutex()),
	mPhysicsShapeRequestsMutex(new LLMutex())
{}

LLMeshRepoThread::~LLMeshRepoThread()
{}

bool LLMeshRepoThread::HeaderRequest::fetch(U32& count)
{
	return gMeshRepo.mThread->fetchMeshHeader(this->mMeshParams,  count);
}

LLMeshRepoThread::MeshRequest::CacheState LLMeshRepoThread::HeaderRequest::readFromVCache()
{
	//look for mesh in asset in vfs
	LLVFile file(gVFS, mMeshParams.getSculptID(), LLAssetType::AT_MESH);

	S32 size = file.getSize();

	if (size > 0)
	{ //NOTE -- if the header size is ever more than 4KB, this will break
		U8 buffer[4096];
		S32 bytes = llmin(size, 4096);
		LLMeshRepository::sCacheBytesRead += bytes;
		file.read(buffer, bytes);
		if (gMeshRepo.mThread->headerReceived(mMeshParams, buffer, bytes, true))
		{
			return VCACHE_HIT;
		}
	}
	return VCACHE_MISS;
}

void LLMeshRepoThread::LODRequest::preFetch()
{
	--LLMeshRepository::sLODProcessing;
}

bool LLMeshRepoThread::LODRequest::fetch(U32& count)
{
	if (!gMeshRepo.mThread->fetchMeshLOD(*this, count))
	{
		++LLMeshRepository::sLODProcessing;
		return false;
	}
	return true;
}

LLMeshRepoThread::MeshRequest::CacheState LLMeshRepoThread::LODRequest::readFromVCache()
{
	const LLUUID& mesh_id = mMeshParams.getSculptID();
	if (gMeshRepo.mThread->getMeshHeaderInfo(mesh_id, header_lod[mLOD].c_str(), mHeaderInfo) && mHeaderInfo.mHeaderSize > 0)
	{
		if (mHeaderInfo.mVersion <= MAX_MESH_VERSION && mHeaderInfo.mOffset >= 0 && mHeaderInfo.mSize > 0)
		{
			if (gMeshRepo.mThread->loadInfoFromVFS(mesh_id, mHeaderInfo, boost::bind(&LLMeshRepoThread::lodReceived, gMeshRepo.mThread, mMeshParams, mLOD, _2, _3)))
				return VCACHE_HIT;
			else
				return VCACHE_MISS;
		}
		return VCACHE_INVALID_HEADER;
	}
	return VCACHE_RETRY;
}

void LLMeshRepoThread::runQueue(std::deque<std::pair<std::shared_ptr<MeshRequest>, F32> >(&queue)[2], U32& count, std::atomic<U32>& active_requests)
{
	const auto canRunVCacheQueue = [&queue]() {return !queue[0].empty(); };
	const auto canRunHttpQueue = [&count, &queue, &active_requests]() {return count < MAX_MESH_REQUESTS_PER_SECOND && (sActiveLODRequests + sActiveHeaderRequests) < sMaxConcurrentRequests && !queue[1].empty(); };
	if (canRunVCacheQueue() || canRunHttpQueue())
	{
		mMutex->lock();
		if (canRunVCacheQueue())
		{
			std::deque<std::pair<std::shared_ptr<MeshRequest>, F32> > incomplete;
			while (canRunVCacheQueue())
			{
				auto entry = queue[0].front();
				queue[0].pop_front();
				auto& req = entry.first;
				mMutex->unlock(); // vfs read can be slow. Don't block.
				auto state = req->readFromVCache();
				mMutex->lock();
				switch (state)
				{
					case MeshRequest::VCACHE_MISS:
						queue[1].push_back(entry);
					case MeshRequest::VCACHE_HIT:
					case MeshRequest::VCACHE_INVALID_HEADER:
						break;
					case MeshRequest::VCACHE_RETRY:
						incomplete.push_back(entry);
					default:
						break;
				}
			}
			while (!incomplete.empty())
			{
				queue[0].push_back(incomplete.front());
				incomplete.pop_front();
			}
		}

		if (canRunHttpQueue())
		{
			std::queue<std::pair<std::shared_ptr<MeshRequest>, F32> > incomplete;
			while (canRunHttpQueue())
			{
				auto entry = queue[1].front();
				queue[1].pop_front();
				auto& req = entry.first;
				F32 delay = entry.second;
				req->preFetch();
				F32 remainder = delay - req->mTimer.getElapsedTimeF32();
				if (remainder > 0.f)
				{
					//LL_INFOS() << req->mMeshParams.getSculptID() << " skipped. " << remainder << "s remaining" << LL_ENDL;
					incomplete.push(std::make_pair(req, delay));
				}
				else if (!req->fetch(count))//failed, resubmit
				{
					LL_INFOS() << req->mMeshParams.getSculptID() << " fetch failed outright. Delaying for " << (delay ? delay : 15) << "s" << LL_ENDL;
					req->mTimer.reset();
					incomplete.push(std::make_pair(req, delay ? delay : 15));
				}
				else {
					//LL_INFOS() << req->mMeshParams.getSculptID() << " fetch request created. " << std::hex << &(req->mMeshParams) << std::dec << LL_ENDL;
				}
			}
			while (!incomplete.empty())
			{
				queue[1].push_back(incomplete.front());
				incomplete.pop();
			}
		}
		mMutex->unlock();
	}
}

void LLMeshRepoThread::runMap(const std::string& type, U32& count, LLMutex& mutex, std::map<LLUUID, MeshHeaderInfo>(&map)[2], parseFn parse_fn, retryFn retry_fn, bool parse_on_invalid)
{
	if (!map[0].empty() || (!map[1].empty() && (count < MAX_MESH_REQUESTS_PER_SECOND && sActiveSubdataRequests < sMaxConcurrentRequests)))
	{
		std::map<LLUUID, MeshHeaderInfo> vfs_map;
		std::map<LLUUID, MeshHeaderInfo> http_map;
		{
			// Swap to local copies.
			LLMutexLock lock(mutex);
			vfs_map.swap(map[0]);
			http_map.swap(map[1]);
		}

		MeshHeaderInfo info;
		for (auto it = vfs_map.begin(); it != vfs_map.end();)
		{
			const LLUUID& mesh_id = it->first;
			if (getMeshHeaderInfo(mesh_id, type.c_str(), info) && info.mHeaderSize > 0)
			{
				if (info.mVersion <= MAX_MESH_VERSION && info.mOffset >= 0 && info.mSize > 0)
				{
					//check VFS for mesh skin info
					bool loaded = loadInfoFromVFS(mesh_id, info, [this, parse_fn](const LLUUID& id, const std::vector<U8>& data, bool from_vfs) {return (this->*parse_fn)(id, data, from_vfs); });
					if (!loaded)
					{
						// Not found. Move into fetch queue.
						http_map.insert(std::make_pair(mesh_id, info));
					}
				}
				else if (parse_on_invalid)
				{
					//Used for physics. If no shape whatsoever, report back empty vector
					(this->*parse_fn)(mesh_id, std::vector<U8>(), false);
				}
				it = vfs_map.erase(it);
			}
			else
			{
				// Keep retrying until there is header info?
				++it;
			}
		}
		
		for (auto it = http_map.begin(); it != http_map.end();)
		{
			if (count >= MAX_MESH_REQUESTS_PER_SECOND || sActiveSubdataRequests >= sMaxConcurrentRequests)
			{
				break;
			}
			else
			{
				const LLUUID& mesh_id = it->first;
				if (!fetchMesh(type, mesh_id, it->second, parse_fn, retry_fn))
				{
					// Error? Retry..
					++it;
				}
				else
				{
					//  Done. Delete.
					it = http_map.erase(it);
				}
			}
		}
		
		if (!vfs_map.empty() || !http_map.empty())
		{
			// Main thread might have added more entries. Lock mutex and simply append.
			LLMutexLock lock(mutex);
			map[0].insert(vfs_map.begin(), vfs_map.end());
			map[1].insert(http_map.begin(), http_map.end());
		}
	}
}

void LLMeshRepoThread::run()
{
	LLCDResult res = LLConvexDecomposition::initThread();
	if (res != LLCD_OK)
	{
		LL_WARNS() << "convex decomposition unable to be loaded" << LL_ENDL;
	}

	mSignal->lock();
	while (!LLApp::isQuitting())
	{
		if (!LLApp::isQuitting())
		{
			static U32 count = 0;

			static F32 last_hundred = gFrameTimeSeconds;

			if (gFrameTimeSeconds - last_hundred > 1.f)
			{	//a second has gone by, clear count
				last_hundred = gFrameTimeSeconds;
				count = 0;	
			}

			mSignal->unlock();
			ms_sleep(1000 / 60);
			mSignal->lock();

			// Favor skin info. Helps reduce amount of meshes stuck in a half-loaded state.
			runMap("skin", count, *mSkinRequestsMutex, mSkinRequests,
				&LLMeshRepoThread::skinInfoReceived,
				&LLMeshRepoThread::retryMeshSkinInfo);

			// NOTE: throttling intentionally favors LOD requests over header requests
			runQueue(mLODReqQ, count, sActiveRequests);
			runQueue(mHeaderReqQ, count, sActiveRequests);

			runMap("physics_convex", count, *mDecompositionRequestsMutex, mDecompositionRequests,
				&LLMeshRepoThread::decompositionReceived,
				&LLMeshRepoThread::retryMeshDecomposition);
			runMap("physics_mesh", count, *mPhysicsShapeRequestsMutex, mPhysicsShapeRequests,
				&LLMeshRepoThread::physicsShapeReceived,
				&LLMeshRepoThread::retryMeshPhysicsShape,
				true);
		}
	}

	if (mSignal->isLocked())
	{ //make sure to let go of the mutex associated with the given signal before shutting down
		mSignal->unlock();
	}

	res = LLConvexDecomposition::quitThread();
	if (res != LLCD_OK)
	{
		LL_WARNS() << "convex decomposition unable to be quit" << LL_ENDL;
	}

}

void LLMeshRepoThread::loadMeshSkinInfo(const LLUUID& mesh_id)
{
	mSkinRequests[0].insert(std::make_pair(mesh_id, MeshHeaderInfo()));
}

void LLMeshRepoThread::retryMeshSkinInfo(const LLUUID& mesh_id, const MeshHeaderInfo& info)
{
	LLMutexLock lock(mSkinRequestsMutex);
	mSkinRequests[1].insert(std::make_pair(mesh_id, info));
}

void LLMeshRepoThread::loadMeshDecomposition(const LLUUID& mesh_id)
{
	mDecompositionRequests[0].insert(std::make_pair(mesh_id, MeshHeaderInfo()));
}

void LLMeshRepoThread::retryMeshDecomposition(const LLUUID& mesh_id, const MeshHeaderInfo& info)
{
	LLMutexLock lock(mDecompositionRequestsMutex);
	mDecompositionRequests[1].insert(std::make_pair(mesh_id, info));
}

void LLMeshRepoThread::loadMeshPhysicsShape(const LLUUID& mesh_id)
{
	mPhysicsShapeRequests[0].insert(std::make_pair(mesh_id, MeshHeaderInfo()));
}

void LLMeshRepoThread::retryMeshPhysicsShape(const LLUUID& mesh_id, const MeshHeaderInfo& info)
{
	LLMutexLock lock(mPhysicsShapeRequestsMutex);
	mPhysicsShapeRequests[1].insert(std::make_pair(mesh_id, info));
}

void LLMeshRepoThread::loadMeshLOD(const LLVolumeParams& mesh_params, S32 lod)
{ //could be called from any thread
	bool found = false;
	{
		LLMutexLock lock(mHeaderMutex);
		mesh_header_map::iterator iter = mMeshHeader.find(mesh_params.getSculptID());
		found = iter != mMeshHeader.end();
	}
	LLMutexLock lock(mMutex);
	if (found)
	{ //if we have the header, request LOD byte range
		gMeshRepo.mThread->pushLODRequest(mesh_params, lod, 0.f);
		++LLMeshRepository::sLODProcessing;
	}
	else
	{ 
		pending_lod_map::iterator pending = mPendingLOD.find(mesh_params);

		if (pending != mPendingLOD.end())
		{	//append this lod request to existing header request
			pending->second.push_back(lod);
			llassert(pending->second.size() <= LLModel::NUM_LODS);
		}
		else
		{	//if no header request is pending, fetch header
			gMeshRepo.mThread->pushHeaderRequest(mesh_params, 0.f);
			mPendingLOD[mesh_params].push_back(lod);
		}
	}
}

//static 
std::string LLMeshRepoThread::constructUrl(LLUUID mesh_id)
{
	std::string http_url;
	
	if (gAgent.getRegion())
	{
		http_url = gMeshRepo.mGetMeshCapability; 
	}

	if (!http_url.empty())
	{
		http_url += "/?mesh_id=";
		http_url += mesh_id.asString().c_str();
	}
	else
	{
		LL_WARNS() << "Current region does not have GetMesh capability!  Cannot load " << mesh_id << ".mesh" << LL_ENDL;
	}

	return http_url;
}

bool LLMeshRepoThread::getMeshHeaderInfo(const LLUUID& mesh_id, const char* block_name, MeshHeaderInfo& info)
{

	if (!mHeaderMutex)
	{
		return false;
	}

	LLMutexLock lock(mHeaderMutex);

	if (mMeshHeader.find(mesh_id) == mMeshHeader.end())
	{ //we have no header info for this mesh, do nothing
		return false;
	}

	if ((info.mHeaderSize = mMeshHeaderSize[mesh_id]) > 0)
	{
		const LLSD& header = mMeshHeader[mesh_id];
		const LLSD& block = header[block_name];
		info.mVersion = header["version"].asInteger();
		info.mOffset = info.mHeaderSize + block["offset"].asInteger();
		info.mSize = block["size"].asInteger();
	}
	return true;
}

bool LLMeshRepoThread::loadInfoFromVFS(const LLUUID& mesh_id, const MeshHeaderInfo& info, std::function<bool(const LLUUID&, const std::vector<U8>&, bool)> parse_fn)
{
	//check VFS for mesh skin info
	LLVFile file(gVFS, mesh_id, LLAssetType::AT_MESH);
	if (file.getSize() >= info.mOffset + info.mSize)
	{
		LLMeshRepository::sCacheBytesRead += info.mSize;

		file.seek(info.mOffset);
		std::vector<U8> data(info.mSize);
		file.read(data.data(), data.size());

		//make sure buffer isn't all 0's by checking the first 1KB (reserved block but not written)
		bool zero = true;
		for (S32 i = 0; i < llmin(info.mSize, S32(1024)) && zero; ++i)
		{
			zero = data[i] == 0;
		}

		if (!zero)
		{	//attempt to parse
			if (parse_fn(mesh_id, data, true))
			{
				return true;
			}
		}
	}
	return false;
}

bool LLMeshRepoThread::fetchMesh(const std::string& type, const LLUUID& mesh_id, const MeshHeaderInfo& info, parseFn parse_fn, retryFn retry_fn)
{
	if (info.mHeaderSize > 0 && info.mVersion <= MAX_MESH_VERSION && info.mOffset >= 0 && info.mSize > 0)
	{
		//reading from VFS failed for whatever reason, fetch from sim
		AIHTTPHeaders headers("Accept", "application/octet-stream");

		std::string http_url = constructUrl(mesh_id);
		if (!http_url.empty())
		{
			if (!LLHTTPClient::getByteRange(http_url, headers, info.mOffset, info.mSize,
				new LLMeshResponder(
					type,
					[this, mesh_id, parse_fn](const std::vector<U8>& data) {return (this->*parse_fn)(mesh_id, data, false); },
					[this, mesh_id, info, retry_fn]() {(this->*retry_fn)(mesh_id, info); },
					mesh_id,
					info,
					&LLMeshRepoThread::sActiveSubdataRequests
			)))
				return false;
			LLMeshRepository::sHTTPRequestCount++;
		}
	}

	//early out was not hit, effectively fetched
	return true;
}

//return false if failed to get header
bool LLMeshRepoThread::fetchMeshHeader(const LLVolumeParams& mesh_params, U32& count)
{
	//either cache entry doesn't exist or is corrupt, request header from simulator	
	bool retval = true;
	AIHTTPHeaders headers("Accept", "application/octet-stream");

	std::string http_url = constructUrl(mesh_params.getSculptID());
	if (!http_url.empty())
	{
		//grab first 4KB if we're going to bother with a fetch.  Cache will prevent future fetches if a full mesh fits
		//within the first 4KB
		//NOTE -- this will break of headers ever exceed 4KB		
		retval = LLHTTPClient::getByteRange(http_url, headers, 0, 4096, new LLMeshHeaderResponder(mesh_params));
		if (retval)
		{
			LLMeshRepository::sHTTPRequestCount++;
			count++;
		}
	}

	return retval;
}

//return false if failed to get mesh lod.
bool LLMeshRepoThread::fetchMeshLOD(const LODRequest& req, U32& count)
{ 
	const LLVolumeParams& mesh_params = req.mMeshParams;
	const LLUUID& mesh_id = mesh_params.getSculptID();
	const MeshHeaderInfo& info = req.mHeaderInfo;
	const S32& lod = req.mLOD;

	/*MeshHeaderInfo info;
	if (!getMeshHeaderInfo(mesh_id, header_lod[lod].c_str(), info))
	{
		return false;
	}*/
	if (info.mHeaderSize > 0)
	{
		if(info.mVersion <= MAX_MESH_VERSION && info.mOffset >= 0 && info.mSize > 0)
		{
			//check VFS for mesh skin info
			//if (gMeshRepo.mThread->loadInfoFromVFS(mesh_id, info, boost::bind(&LLMeshRepoThread::lodReceived, this, mesh_params, lod, _2)))
			//	return true;

			AIHTTPHeaders headers("Accept", "application/octet-stream");

			std::string http_url = constructUrl(mesh_id);
			if (!http_url.empty())
			{		
				count++;		
				if (!LLHTTPClient::getByteRange(http_url, headers, info.mOffset, info.mSize,
					new LLMeshResponder(
						"lod",
						[mesh_params, lod](const std::vector<U8>& data) {
							return gMeshRepo.mThread->lodReceived(mesh_params, lod, data, false);
						},
						[mesh_params, lod]() {
							gMeshRepo.mThread->pushRetryLODRequest(mesh_params, lod, 10.f);
						},
						mesh_id,
						info,
						&LLMeshRepoThread::sActiveLODRequests
				)))
					return false;
				LLMeshRepository::sHTTPRequestCount++;
			}
			else
			{
				LLMutexLock lock(mResultQueuesMutex);
				mUnavailableQ.push(LODRequest(mesh_params, lod));
			}
		}
		else
		{
			LLMutexLock lock(mResultQueuesMutex);
			mUnavailableQ.push(LODRequest(mesh_params, lod));
		}
	}
	
	return true;
}

bool LLMeshRepoThread::headerReceived(const LLVolumeParams& mesh_params, U8* data, S32 data_size, bool from_vfs)
{
	LLSD header;
	
	U32 header_size = 0;
	if (data_size > 0)
	{
		std::string res_str((char*) data, data_size);

		std::string deprecated_header("<? LLSD/Binary ?>");

		if (res_str.substr(0, deprecated_header.size()) == deprecated_header)
		{
			res_str = res_str.substr(deprecated_header.size()+1, data_size);
			header_size = deprecated_header.size()+1;
		}
		data_size = res_str.size();

		std::istringstream stream(res_str);

		if (!LLSDSerialize::fromBinary(header, stream, data_size))
		{
			//LL_WARNS() << "Mesh header " << (from_vfs ? "vfs" : "http") << " parse error. Not a valid mesh asset!" << LL_ENDL;
			return false;
		}
		else if (from_vfs)
		{
			//LL_WARNS() << "Mesh header read from vfs successfully" << LL_ENDL;
		}

		header_size += stream.tellg();
	}
	else
	{
		LL_INFOS()
			<< "Marking header as non-existent, will not retry." << LL_ENDL;
		header["404"] = 1;
	}

	{
		LLUUID mesh_id = mesh_params.getSculptID();
		
		{
			LLMutexLock lock(mHeaderMutex);
			mMeshHeaderSize[mesh_id] = header_size;
			mMeshHeader[mesh_id] = header;
		}

		LLMutexLock lock(mMutex); // make sure only one thread access mPendingLOD at the same time.

		//check for pending requests
		pending_lod_map::iterator iter = mPendingLOD.find(mesh_params);
		if (iter != mPendingLOD.end())
		{
			for (U32 i = 0; i < iter->second.size(); ++i)
			{
				++LLMeshRepository::sLODProcessing;
				gMeshRepo.mThread->pushLODRequest(mesh_params, iter->second[i], 0.f);
			}
			mPendingLOD.erase(iter);
		}
	}

	return true;
}

bool LLMeshRepoThread::lodReceived(const LLVolumeParams& mesh_params, S32 lod, const std::vector<U8>& data, bool from_vfs)
{
	AIStateMachine::StateTimer timer("lodReceived");
	LLPointer<LLVolume> volume = new LLVolume(mesh_params, LLVolumeLODGroup::getVolumeScaleFromDetail(lod));
	std::string mesh_string((char*) data.data(), data.size());
	std::istringstream stream(mesh_string);

	AIStateMachine::StateTimer timer2("unpackVolumeFaces");
	if (volume->unpackVolumeFaces(stream, data.size()))
	{
		AIStateMachine::StateTimer timer("getNumFaces");
		if (volume->getNumFaces() > 0)
		{
			AIStateMachine::StateTimer timer("LoadedMesh");
			LoadedMesh mesh(volume, mesh_params, lod);
			{
				AIStateMachine::StateTimer timer("LLMutexLock");
				LLMutexLock lock(mResultQueuesMutex);
				mLoadedQ.push(mesh);
			}
			//LL_WARNS() << "Mesh lod " << lod << " read from vfs successfully" << LL_ENDL;
			return true;
		}
	}

	//LL_WARNS() << "Mesh lod " << lod << " " << mesh_params.getSculptID() << " " << (from_vfs ? "vfs" : "http") << " parse error. Not a valid mesh asset!" << LL_ENDL;
	return false;
}

bool LLMeshRepoThread::skinInfoReceived(const LLUUID& mesh_id, const std::vector<U8>& data, bool from_vfs)
{
	LLSD skin;

	if (!data.empty())
	{
		std::string res_str((char*)data.data(), data.size());

		std::istringstream stream(res_str);

		if (!unzip_llsd(skin, stream, data.size()))
		{
			//LL_WARNS() << "Mesh skin info " << mesh_id << " " << (from_vfs ? "vfs" : "http") << " parse error. Not a valid mesh asset!" << LL_ENDL;
			return false;
		}
		else if (from_vfs)
		{
			//LL_WARNS() << "Mesh skin info read from vfs successfully" << LL_ENDL;
		}
	}
	
	{
		LLMeshSkinInfo info(skin);
		info.mMeshID = mesh_id;

		//LL_INFOS() <<"info pelvis offset"<<info.mPelvisOffset<<LL_ENDL;
		{
			LLMutexLock lock(mResultQueuesMutex);
			mSkinInfoQ.push(info);
		}
	}

	return true;
}

bool LLMeshRepoThread::decompositionReceived(const LLUUID& mesh_id, const std::vector<U8>& data, bool from_vfs)
{
	LLSD decomp;

	if (!data.empty())
	{ 
		std::string res_str((char*) data.data(), data.size());

		std::istringstream stream(res_str);

		if (!unzip_llsd(decomp, stream, data.size()))
		{
			//LL_WARNS() << "Mesh decomposition " << (from_vfs ? "vfs" : "http") << " parse error. Not a valid mesh asset!" << LL_ENDL;
			return false;
		}
		else if (from_vfs)
		{
			//LL_WARNS() << "Mesh decomposition info read from vfs successfully" << LL_ENDL;
		}
	}
	
	{
		LLModel::Decomposition* d = new LLModel::Decomposition(decomp);
		d->mMeshID = mesh_id;
		LLMutexLock lock(mResultQueuesMutex);
		mDecompositionQ.push(d);
	}

	return true;
}

bool LLMeshRepoThread::physicsShapeReceived(const LLUUID& mesh_id, const std::vector<U8>& data, bool from_vfs)
{
	LLSD physics_shape;

	LLModel::Decomposition* d = new LLModel::Decomposition();
	d->mMeshID = mesh_id;

	if (data.empty())
	{	//no data, no physics shape exists
		d->mPhysicsShapeMesh.clear();
	}
	else
	{
		LLVolumeParams volume_params;
		volume_params.setType(LL_PCODE_PROFILE_SQUARE, LL_PCODE_PATH_LINE);
		volume_params.setSculptID(mesh_id, LL_SCULPT_TYPE_MESH);
		LLPointer<LLVolume> volume = new LLVolume(volume_params,0);
		std::string mesh_string((char*)data.data(), data.size());
		std::istringstream stream(mesh_string);

		if (volume->unpackVolumeFaces(stream, data.size()))
		{
			//load volume faces into decomposition buffer
			S32 vertex_count = 0;
			S32 index_count = 0;

			for (S32 i = 0; i < volume->getNumVolumeFaces(); ++i)
			{
				const LLVolumeFace& face = volume->getVolumeFace(i);
				vertex_count += face.mNumVertices;
				index_count += face.mNumIndices;
			}

			d->mPhysicsShapeMesh.clear();

			std::vector<LLVector3>& pos = d->mPhysicsShapeMesh.mPositions;
			std::vector<LLVector3>& norm = d->mPhysicsShapeMesh.mNormals;

			for (S32 i = 0; i < volume->getNumVolumeFaces(); ++i)
			{
				const LLVolumeFace& face = volume->getVolumeFace(i);
			
				for (S32 i = 0; i < face.mNumIndices; ++i)
				{
					U16 idx = face.mIndices[i];

					pos.push_back(LLVector3(face.mPositions[idx].getF32ptr()));
					norm.push_back(LLVector3(face.mNormals[idx].getF32ptr()));				
				}			
			}
		}
	}

	LLMutexLock lock(mResultQueuesMutex);
	mDecompositionQ.push(d);
	return true;
}

void LLMeshUploadThread::init(LLMeshUploadThread::instance_list& data, LLVector3& scale, bool upload_textures,
							  bool upload_skin, bool upload_joints, bool do_upload,
							  LLHandle<LLWholeModelFeeObserver> const& fee_observer, LLHandle<LLWholeModelUploadObserver> const& upload_observer)
{
	mDoUpload = do_upload;
	mFeeObserverHandle = fee_observer;
	mUploadObserverHandle = upload_observer;
	mInstanceList = data;
	mUploadTextures = upload_textures;
	mUploadSkin = upload_skin;
	mUploadJoints = upload_joints;
	mOrigin = gAgent.getPositionAgent();
	mHost = gAgent.getRegionHost();
	
	mWholeModelFeeCapability = gAgent.getRegion()->getCapability("NewFileAgentInventory");

	mOrigin += gAgent.getAtAxis() * scale.magVec();

	mMeshUploadTimeOut = gSavedSettings.getS32("MeshUploadTimeOut") ;
}

LLMeshUploadThread::~LLMeshUploadThread()
{

}

LLMeshUploadThread::DecompRequest::DecompRequest(LLModel* mdl, LLModel* base_model, LLMeshUploadThread* thread)
{
	mStage = "single_hull";
	mModel = mdl;
	mDecompID = &mdl->mDecompID;
	mBaseModel = base_model;
	mThread = thread;
	
	//copy out positions and indices
	assignData(mdl) ;	

	mThread->mFinalDecomp = this;
	mThread->mPhysicsComplete = false;
}

void LLMeshUploadThread::DecompRequest::completed()
{
	if (mThread->mFinalDecomp == this)
	{
		mThread->mPhysicsComplete = true;
	}

	llassert(mHull.size() == 1);
	
	mThread->mHullMap[mBaseModel] = mHull[0];
}

//called in the main thread.
void LLMeshUploadThread::preStart()
{
	//build map of LLModel refs to instances for callbacks
	for (instance_list::iterator iter = mInstanceList.begin(); iter != mInstanceList.end(); ++iter)
	{
		mInstance[iter->mModel].push_back(*iter);
	}
}

AIMeshUpload::AIMeshUpload(LLMeshUploadThread::instance_list& data, LLVector3& scale, bool upload_textures, bool upload_skin, bool upload_joints, std::string const& upload_url, bool do_upload,
	LLHandle<LLWholeModelFeeObserver> const& fee_observer, LLHandle<LLWholeModelUploadObserver> const& upload_observer) :
#ifdef CWDEBUG
		AIStateMachine(false),
#endif
		mMeshUpload(new AIStateMachineThread<LLMeshUploadThread>(CWD_ONLY(false))), mWholeModelUploadURL(upload_url)
{
	mMeshUpload->thread_impl().init(data, scale, upload_textures, upload_skin, upload_joints, do_upload, fee_observer, upload_observer);
}

char const* AIMeshUpload::state_str_impl(state_type run_state) const
{
	switch (run_state)
	{
		AI_CASE_RETURN(AIMeshUpload_start);
		AI_CASE_RETURN(AIMeshUpload_threadFinished);
		AI_CASE_RETURN(AIMeshUpload_responderFinished);
	}
	return "UNKNOWN STATE";
}

void AIMeshUpload::initialize_impl()
{
	mMeshUpload->thread_impl().preStart();
	set_state(AIMeshUpload_start);
}

void AIMeshUpload::multiplex_impl(state_type run_state)
{
	switch (run_state)
	{
		case AIMeshUpload_start:
			mMeshUpload->run(this, AIMeshUpload_threadFinished);
			idle();										// Wait till the thread finished.
			break;
		case AIMeshUpload_threadFinished:
			mMeshUpload->thread_impl().postRequest(mWholeModelUploadURL, this);
			idle();										// Wait till the responder finished.
			break;
		case AIMeshUpload_responderFinished:
			finish();
			break;
	}
}

bool LLMeshUploadThread::run()
{
	generateHulls();
	wholeModelToLLSD(mModelData, mDoUpload);
	if (!mDoUpload)
	{
		++dump_num;
		dump_llsd_to_file(mModelData, make_dump_name("whole_model_fee_request_", dump_num));
	}
	else
	{
		mBody = mModelData["asset_resources"];
		dump_llsd_to_file(mBody, make_dump_name("whole_model_body_", dump_num));
	}
	return true;          // true = finish, false = abort.
}

void LLMeshUploadThread::postRequest(std::string& whole_model_upload_url, AIMeshUpload* state_machine)
{
	if (mDoUpload)
	{
		LLHTTPClient::post(whole_model_upload_url, mBody,
			new LLWholeModelUploadResponder(mModelData, mUploadObserverHandle)/*,*/
			DEBUG_CURLIO_PARAM(debug_off), keep_alive, state_machine, AIMeshUpload_responderFinished);
	}
	else
	{
		LLHTTPClient::post(mWholeModelFeeCapability, mModelData,
			new LLWholeModelFeeResponder(mModelData, mFeeObserverHandle, whole_model_upload_url)/*,*/
			DEBUG_CURLIO_PARAM(debug_on), keep_alive, state_machine, AIMeshUpload_responderFinished);
	}
}

void dump_llsd_to_file(const LLSD& content, std::string filename)
{
	if (gSavedSettings.getBOOL("MeshUploadLogXML"))
	{
		std::ofstream of(filename.c_str());
		LLSDSerialize::toPrettyXML(content,of);
	}
}

LLSD llsd_from_file(std::string filename)
{
	std::ifstream ifs(filename.c_str());
	LLSD result;
	LLSDSerialize::fromXML(result,ifs);
	return result;
}

void LLMeshUploadThread::wholeModelToLLSD(LLSD& dest, bool include_textures)
{
	LLSD result;

	LLSD res;
	result["folder_id"] = gInventory.findCategoryUUIDForType(LLFolderType::FT_OBJECT);
	result["texture_folder_id"] = gInventory.findCategoryUUIDForType(LLFolderType::FT_TEXTURE);
	result["asset_type"] = "mesh";
	result["inventory_type"] = "object";
	result["description"] = "(No Description)";
	result["next_owner_mask"] = LLSD::Integer(LLFloaterPerms::getNextOwnerPerms("Uploads"));
	result["group_mask"] = LLSD::Integer(LLFloaterPerms::getGroupPerms("Uploads"));
	result["everyone_mask"] = LLSD::Integer(LLFloaterPerms::getEveryonePerms("Uploads"));

	res["mesh_list"] = LLSD::emptyArray();
	res["texture_list"] = LLSD::emptyArray();
	res["instance_list"] = LLSD::emptyArray();
	S32 mesh_num = 0;
	S32 texture_num = 0;
	
	std::set<LLViewerTexture* > textures;
	std::map<LLViewerTexture*,S32> texture_index;

	std::map<LLModel*,S32> mesh_index;
	std::string model_name;
	std::string model_metric;

	S32 instance_num = 0;
	
	for (instance_map::iterator iter = mInstance.begin(); iter != mInstance.end(); ++iter)
	{
		LLMeshUploadData data;
		data.mBaseModel = iter->first;

		if (data.mBaseModel->mSubmodelID)
		{
			// These are handled below to insure correct parenting order on creation
			// due to map walking being based on model address (aka random)
			continue;
		}
		LLModelInstance& first_instance = *(iter->second.begin());
		for (S32 i = 0; i < 5; i++)
		{
			data.mModel[i] = first_instance.mLOD[i];
		}

		if (mesh_index.find(data.mBaseModel) == mesh_index.end())
		{
			// Have not seen this model before - create a new mesh_list entry for it.
			if (model_name.empty())
			{
				model_name = data.mBaseModel->getName();
			}

			if (model_metric.empty())
			{
				model_metric = data.mBaseModel->getMetric();
			}

			std::stringstream ostr;
			
			LLModel::Decomposition& decomp =
				data.mModel[LLModel::LOD_PHYSICS].notNull() ? 
				data.mModel[LLModel::LOD_PHYSICS]->mPhysics : 
				data.mBaseModel->mPhysics;

			decomp.mBaseHull = mHullMap[data.mBaseModel];

			LLSD mesh_header = LLModel::writeModel(
				ostr,  
				data.mModel[LLModel::LOD_PHYSICS],
				data.mModel[LLModel::LOD_HIGH],
				data.mModel[LLModel::LOD_MEDIUM],
				data.mModel[LLModel::LOD_LOW],
				data.mModel[LLModel::LOD_IMPOSTOR], 
				decomp,
				mUploadSkin,
				mUploadJoints,
				FALSE,
				FALSE,
				data.mBaseModel->mSubmodelID);

			data.mAssetData = ostr.str();
			std::string str = ostr.str();

			res["mesh_list"][mesh_num] = LLSD::Binary(str.begin(),str.end()); 
			mesh_index[data.mBaseModel] = mesh_num;
			mesh_num++;
		}

		// For all instances that use this model
		for (instance_list::iterator instance_iter = iter->second.begin();
			 instance_iter != iter->second.end();
			 ++instance_iter)
		{

			LLModelInstance& instance = *instance_iter;
		
			LLSD instance_entry;
		
			for (S32 i = 0; i < 5; i++)
			{
				data.mModel[i] = instance.mLOD[i];
			}
		
			LLVector3 pos, scale;
			LLQuaternion rot;
			LLMatrix4 transformation = instance.mTransform;
			decomposeMeshMatrix(transformation,pos,rot,scale);
			instance_entry["position"] = ll_sd_from_vector3(pos);
			instance_entry["rotation"] = ll_sd_from_quaternion(rot);
			instance_entry["scale"] = ll_sd_from_vector3(scale);
		
			instance_entry["material"] = LL_MCODE_WOOD;
			instance_entry["physics_shape_type"] = data.mModel[LLModel::LOD_PHYSICS].notNull() ? (U8)(LLViewerObject::PHYSICS_SHAPE_PRIM) : (U8)(LLViewerObject::PHYSICS_SHAPE_CONVEX_HULL);
			instance_entry["mesh"] = mesh_index[data.mBaseModel];

			instance_entry["face_list"] = LLSD::emptyArray();

			// We want to be able to allow more than 8 materials...
			//
			S32 end = llmin((S32)instance.mMaterial.size(), instance.mModel->getNumVolumeFaces()) ;

			for (S32 face_num = 0; face_num < end; face_num++)
			{
				LLImportMaterial& material = instance.mMaterial[data.mBaseModel->mMaterialList[face_num]];
				LLSD face_entry = LLSD::emptyMap();

				LLViewerFetchedTexture *texture = NULL;

				if (material.mDiffuseMapFilename.size())
				{
					texture = FindViewerTexture(material);
				}
				
				if ((texture != NULL) &&
					(textures.find(texture) == textures.end()))
				{
					textures.insert(texture);
				}

				std::stringstream texture_str;
				if (texture != NULL && include_textures && mUploadTextures)
				{
					if(texture->hasSavedRawImage())
					{											
						LLPointer<LLImageJ2C> upload_file =
							LLViewerTextureList::convertToUploadFile(texture->getSavedRawImage());

						if (!upload_file.isNull() && upload_file->getDataSize())
						{
						texture_str.write((const char*) upload_file->getData(), upload_file->getDataSize());
					}
				}
				}

				if (texture != NULL &&
					mUploadTextures &&
					texture_index.find(texture) == texture_index.end())
				{
					texture_index[texture] = texture_num;
					std::string str = texture_str.str();
					res["texture_list"][texture_num] = LLSD::Binary(str.begin(),str.end());
					texture_num++;
				}

				// Subset of TextureEntry fields.
				if (texture != NULL && mUploadTextures)
				{
					face_entry["image"] = texture_index[texture];
					face_entry["scales"] = 1.0;
					face_entry["scalet"] = 1.0;
					face_entry["offsets"] = 0.0;
					face_entry["offsett"] = 0.0;
					face_entry["imagerot"] = 0.0;
				}
				face_entry["diffuse_color"] = ll_sd_from_color4(material.mDiffuseColor);
				face_entry["fullbright"] = material.mFullbright;
				instance_entry["face_list"][face_num] = face_entry;
		    }

			res["instance_list"][instance_num] = instance_entry;
			instance_num++;
		}
	}

	for (instance_map::iterator iter = mInstance.begin(); iter != mInstance.end(); ++iter)
	{
		LLMeshUploadData data;
		data.mBaseModel = iter->first;

		if (!data.mBaseModel->mSubmodelID)
		{
			// These were handled above already...
			//
			continue;
		}

		LLModelInstance& first_instance = *(iter->second.begin());
		for (S32 i = 0; i < 5; i++)
		{
			data.mModel[i] = first_instance.mLOD[i];
		}

		if (mesh_index.find(data.mBaseModel) == mesh_index.end())
		{
			// Have not seen this model before - create a new mesh_list entry for it.
			if (model_name.empty())
			{
				model_name = data.mBaseModel->getName();
			}

			if (model_metric.empty())
			{
				model_metric = data.mBaseModel->getMetric();
			}

			std::stringstream ostr;
			
			LLModel::Decomposition& decomp =
				data.mModel[LLModel::LOD_PHYSICS].notNull() ? 
				data.mModel[LLModel::LOD_PHYSICS]->mPhysics : 
				data.mBaseModel->mPhysics;

			decomp.mBaseHull = mHullMap[data.mBaseModel];

			LLSD mesh_header = LLModel::writeModel(
				ostr,  
				data.mModel[LLModel::LOD_PHYSICS],
				data.mModel[LLModel::LOD_HIGH],
				data.mModel[LLModel::LOD_MEDIUM],
				data.mModel[LLModel::LOD_LOW],
				data.mModel[LLModel::LOD_IMPOSTOR], 
				decomp,
				mUploadSkin,
				mUploadJoints,
				FALSE,
				FALSE,
				data.mBaseModel->mSubmodelID);

			data.mAssetData = ostr.str();
			std::string str = ostr.str();

			res["mesh_list"][mesh_num] = LLSD::Binary(str.begin(),str.end()); 
			mesh_index[data.mBaseModel] = mesh_num;
			mesh_num++;
		}

		// For all instances that use this model
		for (instance_list::iterator instance_iter = iter->second.begin();
			 instance_iter != iter->second.end();
			 ++instance_iter)
		{

			LLModelInstance& instance = *instance_iter;
		
			LLSD instance_entry;
		
			for (S32 i = 0; i < 5; i++)
			{
				data.mModel[i] = instance.mLOD[i];
			}
		
			LLVector3 pos, scale;
			LLQuaternion rot;
			LLMatrix4 transformation = instance.mTransform;
			decomposeMeshMatrix(transformation,pos,rot,scale);
			instance_entry["position"] = ll_sd_from_vector3(pos);
			instance_entry["rotation"] = ll_sd_from_quaternion(rot);
			instance_entry["scale"] = ll_sd_from_vector3(scale);
		
			instance_entry["material"] = LL_MCODE_WOOD;
			instance_entry["physics_shape_type"] = (U8)(LLViewerObject::PHYSICS_SHAPE_NONE);
			instance_entry["mesh"] = mesh_index[data.mBaseModel];

			instance_entry["face_list"] = LLSD::emptyArray();

			// We want to be able to allow more than 8 materials...
			//
			S32 end = llmin((S32)instance.mMaterial.size(), instance.mModel->getNumVolumeFaces()) ;

			for (S32 face_num = 0; face_num < end; face_num++)
			{
				LLImportMaterial& material = instance.mMaterial[data.mBaseModel->mMaterialList[face_num]];
				LLSD face_entry = LLSD::emptyMap();

				LLViewerFetchedTexture *texture = NULL;

				if (material.mDiffuseMapFilename.size())
				{
					texture = FindViewerTexture(material);
				}

				if ((texture != NULL) &&
					(textures.find(texture) == textures.end()))
				{
					textures.insert(texture);
				}

				std::stringstream texture_str;
				if (texture != NULL && include_textures && mUploadTextures)
				{
					if(texture->hasSavedRawImage())
					{											
						LLPointer<LLImageJ2C> upload_file =
							LLViewerTextureList::convertToUploadFile(texture->getSavedRawImage());

						if (!upload_file.isNull() && upload_file->getDataSize())
						{
						texture_str.write((const char*) upload_file->getData(), upload_file->getDataSize());
					}
				}
				}

				if (texture != NULL &&
					mUploadTextures &&
					texture_index.find(texture) == texture_index.end())
				{
					texture_index[texture] = texture_num;
					std::string str = texture_str.str();
					res["texture_list"][texture_num] = LLSD::Binary(str.begin(),str.end());
					texture_num++;
				}

				// Subset of TextureEntry fields.
				if (texture != NULL && mUploadTextures)
				{
					face_entry["image"] = texture_index[texture];
					face_entry["scales"] = 1.0;
					face_entry["scalet"] = 1.0;
					face_entry["offsets"] = 0.0;
					face_entry["offsett"] = 0.0;
					face_entry["imagerot"] = 0.0;
				}
				face_entry["diffuse_color"] = ll_sd_from_color4(material.mDiffuseColor);
				face_entry["fullbright"] = material.mFullbright;
				instance_entry["face_list"][face_num] = face_entry;
		    }

			res["instance_list"][instance_num] = instance_entry;
			instance_num++;
		}
	}

	if (model_name.empty()) model_name = "mesh model";
	result["name"] = model_name;
	if (model_metric.empty()) model_metric = "MUT_Unspecified";
	res["metric"] = model_metric;
	result["asset_resources"] = res;
	dump_llsd_to_file(result,make_dump_name("whole_model_",dump_num));

	dest = result;
}

void LLMeshUploadThread::generateHulls()
{
	bool has_valid_requests = false ;

	for (instance_map::iterator iter = mInstance.begin(); iter != mInstance.end(); ++iter)
	{
		LLMeshUploadData data;
		data.mBaseModel = iter->first;

		LLModelInstance& instance = *(iter->second.begin());

		for (S32 i = 0; i < 5; i++)
		{
			data.mModel[i] = instance.mLOD[i];
		}

		//queue up models for hull generation
		LLModel* physics = NULL;

		if (data.mModel[LLModel::LOD_PHYSICS].notNull())
		{
			physics = data.mModel[LLModel::LOD_PHYSICS];
		}
		else if (data.mModel[LLModel::LOD_LOW].notNull())
		{
			physics = data.mModel[LLModel::LOD_LOW];
		}
		else if (data.mModel[LLModel::LOD_MEDIUM].notNull())
		{
			physics = data.mModel[LLModel::LOD_MEDIUM];
		}
		else
		{
			physics = data.mModel[LLModel::LOD_HIGH];
		}

		llassert(physics != NULL);

		DecompRequest* request = new DecompRequest(physics, data.mBaseModel, this);
		if(request->isValid())
		{
			gMeshRepo.mDecompThread->submitRequest(request);
			has_valid_requests = true ;
		}
	}
		
	if(has_valid_requests)
	{
		while (!mPhysicsComplete)
		{
			apr_sleep(100);
		}
	}	
}


void LLMeshRepoThread::notifyLoadedMeshes()
{
	if (!mResultQueuesMutex)
	{
		return;
	}

	if (! mSkinInfoQ.empty() || ! mDecompositionQ.empty() || ! mLoadedQ.empty() || ! mUnavailableQ.empty())
	{
		if (mResultQueuesMutex->try_lock())
		{
			std::queue<LoadedMesh> loaded_q;
			std::queue<LODRequest> unavailable_q;
			std::queue<LLMeshSkinInfo> skin_info_q;
			std::queue<LLModel::Decomposition*> decomp_q;

			loaded_q.swap(mLoadedQ);
			unavailable_q.swap(mUnavailableQ);
			skin_info_q.swap(mSkinInfoQ);
			decomp_q.swap(mDecompositionQ);

			mResultQueuesMutex->unlock();

			// Process the elements free of the lock
			while (! loaded_q.empty())
			{
				LoadedMesh& mesh = loaded_q.front();
				if (mesh.mVolume && mesh.mVolume->getNumVolumeFaces() > 0)
				{
					gMeshRepo.notifyMeshLoaded(mesh.mMeshParams, mesh.mVolume);
				}
				else
				{
					gMeshRepo.notifyMeshUnavailable(mesh.mMeshParams,
						LLVolumeLODGroup::getVolumeDetailFromScale(mesh.mVolume->getDetail()));
				}
				loaded_q.pop();
			}

			while (! unavailable_q.empty())
			{
				const LODRequest& req = unavailable_q.front();
				gMeshRepo.notifyMeshUnavailable(req.mMeshParams, req.mLOD);
				unavailable_q.pop();
			}

			while (! skin_info_q.empty())
			{
				gMeshRepo.notifySkinInfoReceived(skin_info_q.front());
				skin_info_q.pop();
			}

			while (! decomp_q.empty())
			{
				gMeshRepo.notifyDecompositionReceived(decomp_q.front());
				decomp_q.pop();
			}
		}
	}
}

S32 LLMeshRepoThread::getActualMeshLOD(const LLVolumeParams& mesh_params, S32 lod) 
{	//only ever called from main thread
	LLMutexLock lock(mHeaderMutex);
	mesh_header_map::iterator iter = mMeshHeader.find(mesh_params.getSculptID());

	if (iter != mMeshHeader.end())
	{
		LLSD& header = iter->second;

		return LLMeshRepository::getActualMeshLOD(header, lod);
	}

	return lod;
}

//static
S32 LLMeshRepository::getActualMeshLOD(LLSD& header, S32 lod)
{
	lod = llclamp(lod, 0, 3);

	S32 version = header["version"];

	if (header.has("404") || version > MAX_MESH_VERSION)
	{
		return -1;
	}

	if (header[header_lod[lod]]["size"].asInteger() > 0)
	{
		return lod;
	}

	//search down to find the next available lower lod
	for (S32 i = lod-1; i >= 0; --i)
	{
		if (header[header_lod[i]]["size"].asInteger() > 0)
		{
			return i;
		}
	}

	//search up to find then ext available higher lod
	for (S32 i = lod+1; i < 4; ++i)
	{
		if (header[header_lod[i]]["size"].asInteger() > 0)
		{
			return i;
		}
	}

	//header exists and no good lod found, treat as 404
	header["404"] = 1;
	return -1;
}

void LLMeshRepository::cacheOutgoingMesh(LLMeshUploadData& data, LLSD& header)
{
	mThread->mMeshHeader[data.mUUID] = header;

	// we cache the mesh for default parameters
	LLVolumeParams volume_params;
	volume_params.setType(LL_PCODE_PROFILE_SQUARE, LL_PCODE_PATH_LINE);
	volume_params.setSculptID(data.mUUID, LL_SCULPT_TYPE_MESH);

	for (U32 i = 0; i < 4; i++)
	{
		if (data.mModel[i].notNull())
		{
			LLPointer<LLVolume> volume = new LLVolume(volume_params, LLVolumeLODGroup::getVolumeScaleFromDetail(i));
			volume->copyVolumeFaces(data.mModel[i]);
			volume->setMeshAssetLoaded(TRUE);
		}
	}

}

void LLMeshResponder::retry()
{
	if (LLAppViewer::isQuitting())
		return;
	AIStateMachine::StateTimer timer("Retry");
	LLMeshRepository::sHTTPRetryCount++;
	mRetry();
}

void LLMeshResponder::completedRaw(LLChannelDescriptors const& channels,
	LLIOPipe::buffer_ptr_t const& buffer)
{
	mProcessed = true;

	// thread could have already be destroyed during logout
	if (!gMeshRepo.mThread)
	{
		return;
	}

	S32 data_size = buffer->countAfter(channels.in(), NULL);

	if (mStatus < 200 || mStatus >= 400)
	{
		if (is_internal_http_error_that_warrants_a_retry(mStatus) || mStatus == HTTP_SERVICE_UNAVAILABLE)
		{	//timeout or service unavailable, try again
			LL_WARNS() << "Timeout or service unavailable, retrying." << LL_ENDL;
			retry();
		}
		else
		{
			llassert(is_internal_http_error_that_warrants_a_retry(mStatus) || mStatus == HTTP_SERVICE_UNAVAILABLE); //intentionally trigger a breakpoint
			LL_WARNS() << "Unhandled status " << mStatus << LL_ENDL;
		}
		return;
	}

	if (data_size < (S32)mRequestedBytes)
	{
		LL_WARNS() << "Recived insufficient data." << LL_ENDL;
		retry();
		return;
	}

	S32 size = mRequestedBytes;
	LLMeshRepository::sBytesReceived += size;

	std::vector<U8> data;

	if (size > 0)
	{
		AIStateMachine::StateTimer timer("readAfter");
		data.resize(size);
		buffer->readAfter(channels.in(), NULL, data.data(), size);
	}

	if (mHandler(data))
	{
		AIStateMachine::StateTimer timer("FileOpen");
		//good fetch from sim, write to VFS for caching
		LLVFile file(gVFS, mMeshID, LLAssetType::AT_MESH, LLVFile::WRITE);

		S32 offset = mOffset;

		if (file.getSize() >= offset + size)
		{
			AIStateMachine::StateTimer timer("WriteData");
			file.seek(offset);
			file.write(data.data(), size);
			LLMeshRepository::sCacheBytesWritten += size;
		}
	}
}

void LLMeshHeaderResponder::retry()
{
	if (LLAppViewer::isQuitting())
		return;
	AIStateMachine::StateTimer timer("Retry");
	LLMeshRepository::sHTTPRetryCount++;
	gMeshRepo.mThread->pushRetryHeaderRequest(mMeshParams, 10.f);
}

void LLMeshHeaderResponder::completedRaw(LLChannelDescriptors const& channels,
										 LLIOPipe::buffer_ptr_t const& buffer)
{
	//LL_INFOS() << mMeshParams.getSculptID() << " Status: " << mStatus << LL_ENDL;
	mProcessed = true;

	// thread could have already be destroyed during logout
	if( !gMeshRepo.mThread )
	{
		return;
	}

	if (mStatus < 200 || mStatus >= 400)
	{
		if (is_internal_http_error_that_warrants_a_retry(mStatus) || mStatus == HTTP_SERVICE_UNAVAILABLE)
		{
			LL_WARNS() << "Timeout or service unavailable, retrying." << LL_ENDL;
			retry();
			return;
		}
		else
		{
			LL_WARNS() << "Unhandled status: " << mStatus << LL_ENDL;
		}
	}

	S32 data_size = buffer->countAfter(channels.in(), NULL);

	static const U32 BUFF_MAX_STATIC_SIZE = 16384;	//If we exceed this size just bump the vector back to BUFF_MAX_STATIC_SIZE after we're done.
	static std::vector<U8> data(BUFF_MAX_STATIC_SIZE);
	if (data_size > (S32)data.size())
		data.resize(data_size);
	else
		memset(&data[0] + data_size, 0, data.size() - data_size);

	if (data_size > 0)
	{
		AIStateMachine::StateTimer timer("readAfter");
		buffer->readAfter(channels.in(), NULL, &data[0], data_size);
	}

	LLMeshRepository::sBytesReceived += llmin(data_size, 4096);

	AIStateMachine::StateTimer timer("headerReceived");
	bool success = gMeshRepo.mThread->headerReceived(mMeshParams, &data[0], data_size, false);
	
	llassert(success);

	if (!success)
	{
		LL_WARNS()
			<< "Unable to parse mesh header: "
			<< mStatus << ": " << mReason << LL_ENDL;
	}
	else if (data_size > 0)
	{
		//header was successfully retrieved from sim, cache in vfs
		const LLUUID mesh_id = mMeshParams.getSculptID();
		gMeshRepo.mThread->mHeaderMutex->lock();
		const LLSD header = gMeshRepo.mThread->mMeshHeader[mesh_id];
		const S32 header_bytes = (S32)gMeshRepo.mThread->mMeshHeaderSize[mesh_id];
		gMeshRepo.mThread->mHeaderMutex->unlock();

		S32 version = header["version"].asInteger();

		if (version <= MAX_MESH_VERSION)
		{
			std::stringstream str;

			S32 lod_bytes = 0;

			for (U32 i = 0; i < LLModel::LOD_PHYSICS; ++i)
			{ //figure out how many bytes we'll need to reserve in the file
				std::string lod_name = header_lod[i];
				lod_bytes = llmax(lod_bytes, header[lod_name]["offset"].asInteger()+header[lod_name]["size"].asInteger());
			}
		
			//just in case skin info or decomposition is at the end of the file (which it shouldn't be)
			lod_bytes = llmax(lod_bytes, header["skin"]["offset"].asInteger() + header["skin"]["size"].asInteger());
			lod_bytes = llmax(lod_bytes, header["physics_convex"]["offset"].asInteger() + header["physics_convex"]["size"].asInteger());

			S32 bytes = lod_bytes + header_bytes; 
		
			//it's possible for the remote asset to have more data than is needed for the local cache
			//only allocate as much space in the VFS as is needed for the local cache
			data_size = llmin(data_size, bytes);

			AIStateMachine::StateTimer timer("FileOpen");
			LLVFile file(gVFS, mesh_id, LLAssetType::AT_MESH, LLVFile::WRITE);
			if (file.getMaxSize() >= bytes || file.setMaxSize(bytes))
			{
				LLMeshRepository::sCacheBytesWritten += data_size;

				AIStateMachine::StateTimer timer("WriteData");
				S32 bytes_remaining = bytes;
				while (bytes_remaining > 0)
				{
					const S32 bytes_to_write = llmin(bytes_remaining, data_size);
					file.write(&data[0], bytes_to_write);
					if (bytes_remaining == bytes && bytes_to_write < bytes_remaining)
					{
						memset(&data[0], 0, data.size());
					}
					bytes_remaining -= bytes_to_write;
				}
			}
		}
	}

	if (data.size() > BUFF_MAX_STATIC_SIZE)
	{
		std::vector<U8>(BUFF_MAX_STATIC_SIZE).swap(data);
	}
}


LLMeshRepository::LLMeshRepository()
: mMeshMutex(new LLMutex()),
  mMeshThreadCount(0),
  mThread(NULL),
  mDecompThread(nullptr)
{

}

void LLMeshRepository::init()
{
	LLConvexDecomposition::getInstance()->initSystem();

	mDecompThread = new LLPhysicsDecomp();
	mDecompThread->start();

	while (!mDecompThread->mInited)
	{	//wait for physics decomp thread to init
		apr_sleep(100);
	}

	
	
	mThread = new LLMeshRepoThread();
	mThread->start();
}

void LLMeshRepository::shutdown()
{
	LL_INFOS(LOG_MESH) << "Shutting down mesh repository." << LL_ENDL;

	mThread->mSignal->signal();
	
	while (!mThread->isStopped())
	{
		apr_sleep(10);
	}
	delete mThread;
	mThread = NULL;

	LL_INFOS(LOG_MESH) << "Shutting down decomposition system." << LL_ENDL;

	if (mDecompThread)
	{
		mDecompThread->shutdown();		
		delete mDecompThread;
		mDecompThread = NULL;
	}

	LLConvexDecomposition::quitSystem();
}

void LLMeshRepository::unregisterMesh(LLVOVolume* vobj)
{
	LLMutexLock lock(mMeshMutex);
	for (auto& lod : mLoadingMeshes)
	{
		for (auto& param : lod)
		{
			vector_replace_with_last(param.second, vobj);
		}
	}
}

S32 LLMeshRepository::loadMesh(LLVOVolume* vobj, const LLVolumeParams& mesh_params, S32 detail, S32 last_lod)
{
	if (detail < 0 || detail > 4)
	{
		return detail;
	}

	{
		LLMutexLock lock(mMeshMutex);
		//add volume to list of loading meshes
		mesh_load_map::iterator iter = mLoadingMeshes[detail].find(mesh_params);
		if (iter != mLoadingMeshes[detail].end())
		{	//request pending for this mesh, append volume id to list
			auto it = std::find(iter->second.begin(), iter->second.end(), vobj);
			if (it == iter->second.end()) {
				iter->second.push_back(vobj);
			}
		}
		else
		{
			//first request for this mesh
			mLoadingMeshes[detail][mesh_params].push_back(vobj);
			mPendingRequests.push_back(LLMeshRepoThread::LODRequest(mesh_params, detail));
			LLMeshRepository::sLODPending++;
		}
	}

	//do a quick search to see if we can't display something while we wait for this mesh to load
	LLVolume* volume = vobj->getVolume();

	if (volume)
	{
		LLVolumeParams params = volume->getParams();

		LLVolumeLODGroup* group = LLPrimitive::getVolumeManager()->getGroup(params);

		if (group)
		{
			//first, see if last_lod is available (don't transition down to avoid funny popping a la SH-641)
			if (last_lod >= 0)
			{
				LLVolume* lod = group->refLOD(last_lod);
				if (lod && lod->isMeshAssetLoaded() && lod->getNumVolumeFaces() > 0)
				{
					group->derefLOD(lod);
					return last_lod;
				}
				group->derefLOD(lod);
			}

			//next, see what the next lowest LOD available might be
			for (S32 i = detail-1; i >= 0; --i)
			{
				LLVolume* lod = group->refLOD(i);
				if (lod && lod->isMeshAssetLoaded() && lod->getNumVolumeFaces() > 0)
				{
					group->derefLOD(lod);
					return i;
				}

				group->derefLOD(lod);
			}

			//no lower LOD is a available, is a higher lod available?
			for (S32 i = detail+1; i < 4; ++i)
			{
				LLVolume* lod = group->refLOD(i);
				if (lod && lod->isMeshAssetLoaded() && lod->getNumVolumeFaces() > 0)
				{
					group->derefLOD(lod);
					return i;
				}

				group->derefLOD(lod);
			}
		}
	}

	return detail;
}

void LLMeshRepository::notifyLoadedMeshes()
{ //called from main thread
	static const LLCachedControl<U32> max_concurrent_requests("MeshMaxConcurrentRequests");
	LLMeshRepoThread::sMaxConcurrentRequests = max_concurrent_requests;

	//update inventory
	if (!mInventoryQ.empty())
	{
		decltype(mInventoryQ) inventory_q;
		{
			LLMutexLock lock(mMeshMutex);
			inventory_q.swap(mInventoryQ);
		}

		while (!inventory_q.empty())
		{
			inventory_data& data = inventory_q.front();

			LLAssetType::EType asset_type = LLAssetType::lookup(data.mPostData["asset_type"].asString());
			LLInventoryType::EType inventory_type = LLInventoryType::lookup(data.mPostData["inventory_type"].asString());

			// Handle addition of texture, if any.
			if ( data.mResponse.has("new_texture_folder_id") )
			{
				const LLUUID& folder_id = data.mResponse["new_texture_folder_id"].asUUID();

				if ( folder_id.notNull() )
				{
					LLUUID parent_id = gInventory.findCategoryUUIDForType(LLFolderType::FT_TEXTURE);

					std::string name;
					// Check if the server built a different name for the texture folder
					if ( data.mResponse.has("new_texture_folder_name") )
					{
						name = data.mResponse["new_texture_folder_name"].asString();
					}
					else
					{
						name = data.mPostData["name"].asString();
					}

					// Add the category to the internal representation
					LLPointer<LLViewerInventoryCategory> cat = 
						new LLViewerInventoryCategory(folder_id, parent_id, 
							LLFolderType::FT_NONE, name, gAgent.getID());
					cat->setVersion(LLViewerInventoryCategory::VERSION_UNKNOWN);

					LLInventoryModel::LLCategoryUpdate update(cat->getParentUUID(), 1);
					gInventory.accountForUpdate(update);
					gInventory.updateCategory(cat);
				}
			}

			on_new_single_inventory_upload_complete(
				asset_type,
				inventory_type,
				data.mPostData["asset_type"].asString(),
				data.mPostData["folder_id"].asUUID(),
				data.mPostData["name"],
				data.mPostData["description"],
				data.mResponse,
				data.mResponse["upload_price"]);
			//}
			
			inventory_q.pop();
		}
	}

	//call completed callbacks on finished decompositions
	mDecompThread->notifyCompleted();

	if (!mThread->mSignal->try_lock())
	{ //curl thread is churning, wait for it to go idle
		return;
	}
	mThread->mSignal->unlock();

	static std::string region_name("never name a region this");

	if (gAgent.getRegion())
	{ //update capability url 
		if (gAgent.getRegion()->getName() != region_name && gAgent.getRegion()->capabilitiesReceived())
		{
			region_name = gAgent.getRegion()->getName();
			const std::string mesh_cap(gAgent.getRegion()->getViewerAssetUrl());
			mGetMeshCapability = !mesh_cap.empty() ? mesh_cap : gAgent.getRegion()->getCapability("GetMesh2");
			if (mGetMeshCapability.empty())
			{
				mGetMeshCapability = gAgent.getRegion()->getCapability("GetMesh");
			}
		}
	}

	{
		LLMutexLock lock1(mMeshMutex);
		LLMutexLock lock2(mThread->mMutex);
		
		//popup queued error messages from background threads
		while (!mUploadErrorQ.empty())
		{
			LLNotificationsUtil::add("MeshUploadError", mUploadErrorQ.front());
			mUploadErrorQ.pop();
		}

		S32 push_count = LLMeshRepoThread::sMaxConcurrentRequests*2-(LLMeshRepoThread::sActiveHeaderRequests+LLMeshRepoThread::sActiveLODRequests);

		push_count = llmin(push_count, (S32)mPendingRequests.size());

		if (push_count > 0)
		{
			//calculate "score" for pending requests

			//create score map
			boost::unordered_map<LLUUID, F32> score_map;

			for (auto& lod : mLoadingMeshes)
			{
				for (auto& param : lod)
				{
					F32 max_score = 0.f;
					for (auto& vobj : param.second)
					{
						if (LLDrawable* drawable = vobj->mDrawable)
						{
							F32 cur_score = drawable->getRadius() / llmax(drawable->mDistanceWRTCamera, 1.f);
							max_score = llmax(max_score, cur_score);
						}
					}
				
					score_map[param.first.getSculptID()] = max_score;
				}
			}

			//set "score" for pending requests
			for (std::vector<LLMeshRepoThread::LODRequest>::iterator iter = mPendingRequests.begin(); iter != mPendingRequests.end(); ++iter)
			{
				iter->mScore = score_map[iter->mMeshParams.getSculptID()];
			}

			//sort by "score"
			std::partial_sort(mPendingRequests.begin(), mPendingRequests.begin() + push_count,
							  mPendingRequests.end(), LLMeshRepoThread::CompareScoreGreater());

			while (!mPendingRequests.empty() && push_count > 0)
			{
				LLMeshRepoThread::LODRequest& request = mPendingRequests.front();
				mThread->loadMeshLOD(request.mMeshParams, request.mLOD);
				mPendingRequests.erase(mPendingRequests.begin());
				LLMeshRepository::sLODPending--;
				push_count--;
			}
		}
	}

	//send skin info requests
	{
		LLMutexLock lock(mThread->mSkinRequestsMutex);
		while (!mPendingSkinRequests.empty())
		{
			mThread->loadMeshSkinInfo(mPendingSkinRequests.front()); // Guarded
			mPendingSkinRequests.pop();
		}
	}

	//send decomposition requests
	{
		LLMutexLock lock(mThread->mDecompositionRequestsMutex);
		while (!mPendingDecompositionRequests.empty())
		{
			mThread->loadMeshDecomposition(mPendingDecompositionRequests.front()); // Guarded
			mPendingDecompositionRequests.pop();
		}
	}

	//send physics shapes decomposition requests
	{
		LLMutexLock lock(mThread->mPhysicsShapeRequestsMutex);
		while (!mPendingPhysicsShapeRequests.empty())
		{
			mThread->loadMeshPhysicsShape(mPendingPhysicsShapeRequests.front()); // Guarded
			mPendingPhysicsShapeRequests.pop();
		}
	}

	mThread->notifyLoadedMeshes();

	mThread->mSignal->signal();
}

void LLMeshRepository::notifySkinInfoReceived(LLMeshSkinInfo& info)
{
	mSkinMap[info.mMeshID] = std::make_shared<LLMeshSkinInfo>(info);

	skin_load_map::iterator iter = mLoadingSkins.find(info.mMeshID);
	if (iter != mLoadingSkins.end())
	{
		for (std::set<LLUUID>::iterator obj_id = iter->second.begin(); obj_id != iter->second.end(); ++obj_id)
		{
			LLVOVolume* vobj = (LLVOVolume*) gObjectList.findObject(*obj_id);
			if (vobj)
			{
				vobj->notifyMeshLoaded();
			}
		}
		mLoadingSkins.erase(info.mMeshID);
	}
}

void LLMeshRepository::notifyDecompositionReceived(LLModel::Decomposition* decomp)
{
	decomposition_map::iterator iter = mDecompositionMap.find(decomp->mMeshID);
	if (iter == mDecompositionMap.end())
	{	//just insert decomp into map
		mDecompositionMap[decomp->mMeshID] = decomp;
		mLoadingDecompositions.erase(decomp->mMeshID);
	}
	else
	{ //merge decomp with existing entry
		iter->second->merge(decomp);
		mLoadingDecompositions.erase(decomp->mMeshID);
		delete decomp;
	}
}

void LLMeshRepository::notifyMeshLoaded(const LLVolumeParams& mesh_params, LLVolume* volume)
{ //called from main thread
	S32 detail = LLVolumeLODGroup::getVolumeDetailFromScale(volume->getDetail());

	//get list of objects waiting to be notified this mesh is loaded
	mesh_load_map::iterator obj_iter = mLoadingMeshes[detail].find(mesh_params);

	if (volume && obj_iter != mLoadingMeshes[detail].end())
	{
		//make sure target volume is still valid
		if (volume->getNumVolumeFaces() <= 0)
		{
			LL_WARNS(LOG_MESH) << "Mesh loading returned empty volume.  ID:  " << mesh_params.getSculptID()
							   << LL_ENDL;
		}
		
		{ //update system volume
			LLVolume* sys_volume = LLPrimitive::getVolumeManager()->refVolume(mesh_params, detail);
			if (sys_volume)
			{
				sys_volume->copyVolumeFaces(volume);
				sys_volume->setMeshAssetLoaded(TRUE);
				LLPrimitive::getVolumeManager()->unrefVolume(sys_volume);
			}
			else
			{
				LL_WARNS(LOG_MESH) << "Couldn't find system volume for mesh " << mesh_params.getSculptID()
								   << LL_ENDL;
			}
		}

		//notify waiting LLVOVolume instances that their requested mesh is available
		for (auto& vobj : obj_iter->second)
		{
			vobj->notifyMeshLoaded();
		}
		
		mLoadingMeshes[detail].erase(mesh_params);
	}
}

void LLMeshRepository::notifyMeshUnavailable(const LLVolumeParams& mesh_params, S32 lod)
{	//called from main thread
	//get list of objects waiting to be notified this mesh is loaded
	mesh_load_map::iterator obj_iter = mLoadingMeshes[lod].find(mesh_params);

	F32 detail = LLVolumeLODGroup::getVolumeScaleFromDetail(lod);

	if (obj_iter != mLoadingMeshes[lod].end())
	{
		for (auto& vobj : obj_iter->second)
		{
			LLVolume* obj_volume = vobj->getVolume();
			if (obj_volume && 
				obj_volume->getDetail() == detail &&
				obj_volume->getParams() == mesh_params)
			{	//should force volume to find most appropriate LOD
					vobj->setVolume(obj_volume->getParams(), lod);
			}
		}
		
		mLoadingMeshes[lod].erase(mesh_params);
	}
}

S32 LLMeshRepository::getActualMeshLOD(const LLVolumeParams& mesh_params, S32 lod)
{
	return mThread->getActualMeshLOD(mesh_params, lod);
}

const LLMeshSkinInfo* LLMeshRepository::getSkinInfo(const LLUUID& mesh_id, const LLVOVolume* requesting_obj)
{
	if (mesh_id.notNull())
	{
		skin_map::iterator iter = mSkinMap.find(mesh_id);
		if (iter != mSkinMap.end())
		{
			return iter->second.get();
		}
		//mSkinMap[mesh_id] = std::shared_ptr<LLMeshSkinInfo>(nullptr);
		
		//no skin info known about given mesh, try to fetch it
		{
			//add volume to list of loading meshes
			skin_load_map::iterator iter = mLoadingSkins.find(mesh_id);
			if (iter == mLoadingSkins.end())
			{ //no request pending for this skin info
				LLMutexLock lock(mThread->mSkinRequestsMutex);
				mPendingSkinRequests.push(mesh_id); // Guarded
			}
			mLoadingSkins[mesh_id].insert(requesting_obj->getID());
		}
	}

	return NULL;
}

void LLMeshRepository::fetchPhysicsShape(const LLUUID& mesh_id)
{
	if (mesh_id.notNull())
	{
		LLModel::Decomposition* decomp = NULL;
		decomposition_map::iterator iter = mDecompositionMap.find(mesh_id);
		if (iter != mDecompositionMap.end())
		{
			decomp = iter->second;
		}
		
		//decomposition block hasn't been fetched yet
		if (!decomp || decomp->mPhysicsShapeMesh.empty())
		{
			//add volume to list of loading meshes
			std::set<LLUUID>::iterator iter = mLoadingPhysicsShapes.find(mesh_id);
			if (iter == mLoadingPhysicsShapes.end())
			{	//no request pending for this skin info
				mLoadingPhysicsShapes.insert(mesh_id);
				LLMutexLock lock(mThread->mPhysicsShapeRequestsMutex);
				mPendingPhysicsShapeRequests.push(mesh_id); // Guarded
			}
		}
	}

}

LLModel::Decomposition* LLMeshRepository::getDecomposition(const LLUUID& mesh_id)
{
	LLModel::Decomposition* ret = NULL;

	if (mesh_id.notNull())
	{
		decomposition_map::iterator iter = mDecompositionMap.find(mesh_id);
		if (iter != mDecompositionMap.end())
		{
			ret = iter->second;
		}
		
		//decomposition block hasn't been fetched yet
		if (!ret || ret->mBaseHullMesh.empty())
		{
			//add volume to list of loading meshes
			std::set<LLUUID>::iterator iter = mLoadingDecompositions.find(mesh_id);
			if (iter == mLoadingDecompositions.end())
			{	//no request pending for this skin info
				mLoadingDecompositions.insert(mesh_id);
				LLMutexLock lock(mThread->mDecompositionRequestsMutex);
				mPendingDecompositionRequests.push(mesh_id); // Guarded
			}
		}
	}

	return ret;
}

void LLMeshRepository::buildHull(const LLVolumeParams& params, S32 detail)
{
	LLVolume* volume = LLPrimitive::getVolumeManager()->refVolume(params, detail);

	if (!volume->mHullPoints)
	{
		//all default params
		//execute first stage
		//set simplify mode to retain
		//set retain percentage to zero
		//run second stage
	}

	LLPrimitive::getVolumeManager()->unrefVolume(volume);
}

bool LLMeshRepository::hasPhysicsShape(const LLUUID& mesh_id)
{
	LLSD mesh = mThread->getMeshHeader(mesh_id);
	if (mesh.has("physics_mesh") && mesh["physics_mesh"].has("size") && (mesh["physics_mesh"]["size"].asInteger() > 0))
	{
		return true;
	}

	LLModel::Decomposition* decomp = getDecomposition(mesh_id);
	if (decomp && !decomp->mHull.empty())
	{
		return true;
	}

	return false;
}

LLSD& LLMeshRepository::getMeshHeader(const LLUUID& mesh_id)
{
	return mThread->getMeshHeader(mesh_id);
}

LLSD& LLMeshRepoThread::getMeshHeader(const LLUUID& mesh_id)
{
	static LLSD dummy_ret;
	if (mesh_id.notNull())
	{
		LLMutexLock lock(mHeaderMutex);
		mesh_header_map::iterator iter = mMeshHeader.find(mesh_id);
		if (iter != mMeshHeader.end() && mMeshHeaderSize[mesh_id] > 0)
		{
			return iter->second;
		}
	}

	return dummy_ret;
}


void LLMeshRepository::uploadModel(std::vector<LLModelInstance>& data, LLVector3& scale, bool upload_textures,
									bool upload_skin, bool upload_joints, std::string upload_url, bool do_upload,
								   LLHandle<LLWholeModelFeeObserver> fee_observer, LLHandle<LLWholeModelUploadObserver> upload_observer)
{
	if (do_upload && upload_url.empty())
	{
		LL_INFOS() << "unable to upload, fee request failed" << LL_ENDL;
		return;
	}
	AIMeshUpload* thread = new AIMeshUpload(data, scale, upload_textures, upload_skin, upload_joints, upload_url,
												do_upload, fee_observer, upload_observer);
	thread->run(NULL, 0, false, true, &gMainThreadEngine);
}

S32 LLMeshRepository::getMeshSize(const LLUUID& mesh_id, S32 lod)
{
	if (mThread && mesh_id.notNull() && LLPrimitive::NO_LOD != lod)
	{
		LLMutexLock lock(mThread->mHeaderMutex);
		LLMeshRepoThread::mesh_header_map::iterator iter = mThread->mMeshHeader.find(mesh_id);
		if (iter != mThread->mMeshHeader.end() && mThread->mMeshHeaderSize[mesh_id] > 0)
		{
			LLSD& header = iter->second;

			if (header.has("404"))
			{
				return -1;
			}

			S32 size = header[header_lod[lod]]["size"].asInteger();
			return size;
		}

	}

	return -1;

}

void LLMeshUploadThread::decomposeMeshMatrix(LLMatrix4& transformation,
											 LLVector3& result_pos,
											 LLQuaternion& result_rot,
											 LLVector3& result_scale)
{
	// check for reflection
	BOOL reflected = (transformation.determinant() < 0);

	// compute position
	LLVector3 position = LLVector3(0, 0, 0) * transformation;

	// compute scale
	LLVector3 x_transformed = LLVector3(1, 0, 0) * transformation - position;
	LLVector3 y_transformed = LLVector3(0, 1, 0) * transformation - position;
	LLVector3 z_transformed = LLVector3(0, 0, 1) * transformation - position;
	F32 x_length = x_transformed.normalize();
	F32 y_length = y_transformed.normalize();
	F32 z_length = z_transformed.normalize();
	LLVector3 scale = LLVector3(x_length, y_length, z_length);

    // adjust for "reflected" geometry
	LLVector3 x_transformed_reflected = x_transformed;
	if (reflected)
	{
		x_transformed_reflected *= -1.0;
	}
	
	// compute rotation
	LLMatrix3 rotation_matrix;
	rotation_matrix.setRows(x_transformed_reflected, y_transformed, z_transformed);
	LLQuaternion quat_rotation = rotation_matrix.quaternion();
	quat_rotation.normalize(); // the rotation_matrix might not have been orthoginal.  make it so here.
	LLVector3 euler_rotation;
	quat_rotation.getEulerAngles(&euler_rotation.mV[VX], &euler_rotation.mV[VY], &euler_rotation.mV[VZ]);

	result_pos = position + mOrigin;
	result_scale = scale;
	result_rot = quat_rotation; 
}

void LLMeshRepository::updateInventory(inventory_data data)
{
	LLMutexLock lock(mMeshMutex);
	dump_llsd_to_file(data.mPostData,make_dump_name("update_inventory_post_data_",dump_num));
	dump_llsd_to_file(data.mResponse,make_dump_name("update_inventory_response_",dump_num));
	mInventoryQ.push(data);
}

void LLMeshRepository::uploadError(LLSD& args)
{
	LLMutexLock lock(mMeshMutex);
	mUploadErrorQ.push(args);
}

F32 LLMeshRepository::getEstTrianglesMax(LLUUID mesh_id)
{
	LLMeshCostData costs;
	if (getCostData(mesh_id, costs))
	{
		return costs.getEstTrisMax();
	}
	else
	{
		return 0.f;
	}
}

F32 LLMeshRepository::getEstTrianglesStreamingCost(LLUUID mesh_id)
{
    LLMeshCostData costs;
    if (getCostData(mesh_id, costs))
    {
        return costs.getEstTrisForStreamingCost();
    }
    else
    {
        return 0.f;
    }
}

// FIXME replace with calc based on LLMeshCostData
F32 LLMeshRepository::getStreamingCostLegacy(LLUUID mesh_id, F32 radius, S32* bytes, S32* bytes_visible, S32 lod, F32 *unscaled_value)
{
	F32 result = 0.f;
    if (mThread && mesh_id.notNull())
    {
        LLMutexLock lock(mThread->mHeaderMutex);
        LLMeshRepoThread::mesh_header_map::iterator iter = mThread->mMeshHeader.find(mesh_id);
        if (iter != mThread->mMeshHeader.end() && mThread->mMeshHeaderSize[mesh_id] > 0)
        {
            result  = getStreamingCostLegacy(iter->second, radius, bytes, bytes_visible, lod, unscaled_value);
        }
    }
    if (result > 0.f)
    {
        LLMeshCostData data;
        if (getCostData(mesh_id, data))
        {
            F32 ref_streaming_cost = data.getRadiusBasedStreamingCost(radius);
            F32 ref_weighted_tris = data.getRadiusWeightedTris(radius);
            if (!is_approx_equal(ref_streaming_cost,result))
            {
                LL_WARNS() << mesh_id << "streaming mismatch " << result << " " << ref_streaming_cost << LL_ENDL;
            }
            if (unscaled_value && !is_approx_equal(ref_weighted_tris,*unscaled_value))
            {
                LL_WARNS() << mesh_id << "weighted_tris mismatch " << *unscaled_value << " " << ref_weighted_tris << LL_ENDL;
            }
            if (bytes && (*bytes != data.getSizeTotal()))
            {
                LL_WARNS() << mesh_id << "bytes mismatch " << *bytes << " " << data.getSizeTotal() << LL_ENDL;
            }
            if (bytes_visible && (lod >=0) && (lod < 4) && (*bytes_visible != data.getSizeByLOD(lod)))
            {
                LL_WARNS() << mesh_id << "bytes_visible mismatch " << *bytes_visible << " " << data.getSizeByLOD(lod) << LL_ENDL;
            }
        }
        else
        {
            LL_WARNS() << "getCostData failed!!!" << LL_ENDL;
        }
    }
    return result;
}

// FIXME replace with calc based on LLMeshCostData
//static
F32 LLMeshRepository::getStreamingCostLegacy(LLSD& header, F32 radius, S32* bytes, S32* bytes_visible, S32 lod, F32 *unscaled_value)
{
	if (header.has("404")
		|| !header.has("lowest_lod")
		|| (header.has("version") && header["version"].asInteger() > MAX_MESH_VERSION))
	{
		return 0.f;
	}

	F32 max_distance = 512.f;

	F32 dlowest = llmin(radius/0.03f, max_distance);
	F32 dlow = llmin(radius/0.06f, max_distance);
	F32 dmid = llmin(radius/0.24f, max_distance);
	
	static const LLCachedControl<U32> mesh_meta_data_discount("MeshMetaDataDiscount");
	static const LLCachedControl<U32> mesh_minimum_byte_size("MeshMinimumByteSize");
	static const LLCachedControl<U32> mesh_bytes_per_triangle("MeshBytesPerTriangle");
	static const LLCachedControl<U32> mesh_triangle_budget("MeshTriangleBudget");

	F32 METADATA_DISCOUNT = (F32) mesh_meta_data_discount.get();  //discount 128 bytes to cover the cost of LLSD tags and compression domain overhead
	F32 MINIMUM_SIZE = (F32) mesh_minimum_byte_size.get(); //make sure nothing is "free"

	F32 bytes_per_triangle = (F32) mesh_bytes_per_triangle.get();

	S32 bytes_lowest = header["lowest_lod"]["size"].asInteger();
	S32 bytes_low = header["low_lod"]["size"].asInteger();
	S32 bytes_mid = header["medium_lod"]["size"].asInteger();
	S32 bytes_high = header["high_lod"]["size"].asInteger();

	if (bytes_high == 0)
	{
		return 0.f;
	}

	if (bytes_mid == 0)
	{
		bytes_mid = bytes_high;
	}

	if (bytes_low == 0)
	{
		bytes_low = bytes_mid;
	}

	if (bytes_lowest == 0)
	{
		bytes_lowest = bytes_low;
	}

	F32 triangles_lowest = llmax((F32) bytes_lowest-METADATA_DISCOUNT, MINIMUM_SIZE)/bytes_per_triangle;
	F32 triangles_low = llmax((F32) bytes_low-METADATA_DISCOUNT, MINIMUM_SIZE)/bytes_per_triangle;
	F32 triangles_mid = llmax((F32) bytes_mid-METADATA_DISCOUNT, MINIMUM_SIZE)/bytes_per_triangle;
	F32 triangles_high = llmax((F32) bytes_high-METADATA_DISCOUNT, MINIMUM_SIZE)/bytes_per_triangle;

	if (bytes)
	{
		*bytes = 0;
		*bytes += header["lowest_lod"]["size"].asInteger();
		*bytes += header["low_lod"]["size"].asInteger();
		*bytes += header["medium_lod"]["size"].asInteger();
		*bytes += header["high_lod"]["size"].asInteger();
	}

	if (bytes_visible)
	{
		lod = LLMeshRepository::getActualMeshLOD(header, lod);
		if (lod >= 0 && lod <= 3)
		{
			*bytes_visible = header[header_lod[lod]]["size"].asInteger();
		}
	}

	F32 max_area = 102944.f; //area of circle that encompasses region (see MAINT-6559)
	F32 min_area = 1.f;

	F32 high_area = llmin(F_PI*dmid*dmid, max_area);
	F32 mid_area = llmin(F_PI*dlow*dlow, max_area);
	F32 low_area = llmin(F_PI*dlowest*dlowest, max_area);
	F32 lowest_area = max_area;

	lowest_area -= low_area;
	low_area -= mid_area;
	mid_area -= high_area;

	high_area = llclamp(high_area, min_area, max_area);
	mid_area = llclamp(mid_area, min_area, max_area);
	low_area = llclamp(low_area, min_area, max_area);
	lowest_area = llclamp(lowest_area, min_area, max_area);

	F32 total_area = high_area + mid_area + low_area + lowest_area;
	high_area /= total_area;
	mid_area /= total_area;
	low_area /= total_area;
	lowest_area /= total_area;

	F32 weighted_avg = triangles_high*high_area +
					   triangles_mid*mid_area +
					   triangles_low*low_area +
					  triangles_lowest*lowest_area;

	if (unscaled_value)
	{
		*unscaled_value = weighted_avg;
	}

	return weighted_avg/mesh_triangle_budget*15000.f;
}

LLMeshCostData::LLMeshCostData()
{
    mSizeByLOD.resize(4);
    mEstTrisByLOD.resize(4);

    std::fill(mSizeByLOD.begin(), mSizeByLOD.end(), 0);
    std::fill(mEstTrisByLOD.begin(), mEstTrisByLOD.end(), 0.f);
}

bool LLMeshCostData::init(const LLSD& header)
{
    mSizeByLOD.resize(4);
    mEstTrisByLOD.resize(4);

    std::fill(mSizeByLOD.begin(), mSizeByLOD.end(), 0);
    std::fill(mEstTrisByLOD.begin(), mEstTrisByLOD.end(), 0.f);
    
    S32 bytes_high = header["high_lod"]["size"].asInteger();
    S32 bytes_med = header["medium_lod"]["size"].asInteger();
    if (bytes_med == 0)
    {
        bytes_med = bytes_high;
    }
    S32 bytes_low = header["low_lod"]["size"].asInteger();
    if (bytes_low == 0)
    {
        bytes_low = bytes_med;
    }
    S32 bytes_lowest = header["lowest_lod"]["size"].asInteger();
    if (bytes_lowest == 0)
    {
        bytes_lowest = bytes_low;
    }
    mSizeByLOD[0] = bytes_lowest;
    mSizeByLOD[1] = bytes_low;
    mSizeByLOD[2] = bytes_med;
    mSizeByLOD[3] = bytes_high;

    F32 METADATA_DISCOUNT = (F32) gSavedSettings.getU32("MeshMetaDataDiscount");  //discount 128 bytes to cover the cost of LLSD tags and compression domain overhead
    F32 MINIMUM_SIZE = (F32) gSavedSettings.getU32("MeshMinimumByteSize"); //make sure nothing is "free"
    F32 bytes_per_triangle = (F32) gSavedSettings.getU32("MeshBytesPerTriangle");

    for (S32 i=0; i<4; i++)
    {
        mEstTrisByLOD[i] = llmax((F32) mSizeByLOD[i]-METADATA_DISCOUNT, MINIMUM_SIZE)/bytes_per_triangle; 
    }

    return true;
}


S32 LLMeshCostData::getSizeByLOD(S32 lod)
{
    if (llclamp(lod,0,3) != lod)
    {
        return 0;
    }
    return mSizeByLOD[lod];
}

S32 LLMeshCostData::getSizeTotal()
{
    return mSizeByLOD[0] + mSizeByLOD[1] + mSizeByLOD[2] + mSizeByLOD[3];
}

F32 LLMeshCostData::getEstTrisByLOD(S32 lod)
{
    if (llclamp(lod,0,3) != lod)
    {
        return 0.f;
    }
    return mEstTrisByLOD[lod];
}

F32 LLMeshCostData::getEstTrisMax()
{
    return llmax(mEstTrisByLOD[0], mEstTrisByLOD[1], mEstTrisByLOD[2], mEstTrisByLOD[3]);
}

F32 LLMeshCostData::getRadiusWeightedTris(F32 radius)
{
	F32 max_distance = 512.f;

	F32 dlowest = llmin(radius/0.03f, max_distance);
	F32 dlow = llmin(radius/0.06f, max_distance);
	F32 dmid = llmin(radius/0.24f, max_distance);
	
	F32 triangles_lowest = mEstTrisByLOD[0];
	F32 triangles_low = mEstTrisByLOD[1];
	F32 triangles_mid = mEstTrisByLOD[2];
	F32 triangles_high = mEstTrisByLOD[3];

	F32 max_area = 102944.f; //area of circle that encompasses region (see MAINT-6559)
	F32 min_area = 1.f;

	F32 high_area = llmin(F_PI*dmid*dmid, max_area);
	F32 mid_area = llmin(F_PI*dlow*dlow, max_area);
	F32 low_area = llmin(F_PI*dlowest*dlowest, max_area);
	F32 lowest_area = max_area;

	lowest_area -= low_area;
	low_area -= mid_area;
	mid_area -= high_area;

	high_area = llclamp(high_area, min_area, max_area);
	mid_area = llclamp(mid_area, min_area, max_area);
	low_area = llclamp(low_area, min_area, max_area);
	lowest_area = llclamp(lowest_area, min_area, max_area);

	F32 total_area = high_area + mid_area + low_area + lowest_area;
	high_area /= total_area;
	mid_area /= total_area;
	low_area /= total_area;
	lowest_area /= total_area;

	F32 weighted_avg = triangles_high*high_area +
					   triangles_mid*mid_area +
					   triangles_low*low_area +
					   triangles_lowest*lowest_area;

    return weighted_avg;
}

F32 LLMeshCostData::getEstTrisForStreamingCost()
{
    LL_DEBUGS("StreamingCost") << "tris_by_lod: "
                               << mEstTrisByLOD[0] << ", "
                               << mEstTrisByLOD[1] << ", "
                               << mEstTrisByLOD[2] << ", "
                               << mEstTrisByLOD[3] << LL_ENDL;

    F32 charged_tris = mEstTrisByLOD[3];
    F32 allowed_tris = mEstTrisByLOD[3];
    const F32 ENFORCE_FLOOR = 64.0f;
    for (S32 i=2; i>=0; i--)
    {
        // How many tris can we have in this LOD without affecting land impact?
        // - normally an LOD should be at most half the size of the previous one.
        // - once we reach a floor of ENFORCE_FLOOR, don't require LODs to get any smaller.
        allowed_tris = llclamp(allowed_tris/2.0f,ENFORCE_FLOOR,mEstTrisByLOD[i]);
        F32 excess_tris = mEstTrisByLOD[i]-allowed_tris;
        if (excess_tris>0.f)
        {
            LL_DEBUGS("StreamingCost") << "excess tris in lod[" << i << "] " << excess_tris << " allowed " << allowed_tris <<  LL_ENDL;
            charged_tris += excess_tris;
        }
    }
    return charged_tris;
}

F32 LLMeshCostData::getRadiusBasedStreamingCost(F32 radius)
{
	return getRadiusWeightedTris(radius)/gSavedSettings.getU32("MeshTriangleBudget")*15000.f;
}

F32 LLMeshCostData::getTriangleBasedStreamingCost()
{
    F32 result = ANIMATED_OBJECT_COST_PER_KTRI * 0.001 * getEstTrisForStreamingCost();
    return result;
}


bool LLMeshRepository::getCostData(LLUUID mesh_id, LLMeshCostData& data)
{
	data = LLMeshCostData();

	if (mThread && mesh_id.notNull())
	{
		LLMutexLock lock(mThread->mHeaderMutex);
		LLMeshRepoThread::mesh_header_map::iterator iter = mThread->mMeshHeader.find(mesh_id);
		if (iter != mThread->mMeshHeader.end() && mThread->mMeshHeaderSize[mesh_id] > 0)
		{
			const LLSD& header = iter->second;

			bool header_invalid = (header.has("404")
				|| !header.has("lowest_lod")
				|| (header.has("version") && header["version"].asInteger() > MAX_MESH_VERSION));
			if (!header_invalid)
			{
				return getCostData(header, data);
			}

			return true;
		}
	}
	return false;
}
	
bool LLMeshRepository::getCostData(LLSD& header, LLMeshCostData& data)
{
	data = LLMeshCostData();

	if (!data.init(header))
	{
		return false;
	}

	return true;
}
LLPhysicsDecomp::LLPhysicsDecomp()
:	LLThread("Physics Decomp"),
	mSignal(new LLCondition()),
	mMutex(new LLMutex())
{
	mInited = false;
	mQuitting = false;
	mDone = false;
}

LLPhysicsDecomp::~LLPhysicsDecomp()
{
	shutdown();
}

void LLPhysicsDecomp::shutdown()
{
	if (mSignal)
	{
		mQuitting = true;
		mSignal->signal();

		while (!isStopped())
		{
			apr_sleep(10);
		}
	}
}

void LLPhysicsDecomp::submitRequest(LLPhysicsDecomp::Request* request)
{
	LLMutexLock lock(mMutex);
	mRequestQ.push(request);
	mSignal->signal();
}

//static
S32 LLPhysicsDecomp::llcdCallback(const char* status, S32 p1, S32 p2)
{	
	if (gMeshRepo.mDecompThread && gMeshRepo.mDecompThread->mCurRequest.notNull())
	{
		return gMeshRepo.mDecompThread->mCurRequest->statusCallback(status, p1, p2);
	}

	return 1;
}

bool needTriangles( LLConvexDecomposition *aDC )
{
	if( !aDC )
		return false;

	LLCDParam const  *pParams(0);
	int nParams = aDC->getParameters( &pParams );

	if( nParams <= 0 )
		return false;

	for( int i = 0; i < nParams; ++i )
	{
		if( pParams[i].mName && strcmp( "nd_AlwaysNeedTriangles", pParams[i].mName ) == 0 )
		{
			if( LLCDParam::LLCD_BOOLEAN == pParams[i].mType && pParams[i].mDefault.mBool )
				return true;
			else
				return false;
		}
	}

	return false;
}

void LLPhysicsDecomp::setMeshData(LLCDMeshData& mesh, bool vertex_based)
{
	// <singu> HACD
	if (vertex_based)
	{
		if (LLConvexDecomposition* pDeComp = LLConvexDecomposition::getInstance())
			vertex_based = !needTriangles(pDeComp);
		else
			return;
	}
	// </singu>

	mesh.mVertexBase = mCurRequest->mPositions[0].mV;
	mesh.mVertexStrideBytes = 12;
	mesh.mNumVertices = mCurRequest->mPositions.size();

	if(!vertex_based)
	{
		mesh.mIndexType = LLCDMeshData::INT_16;
		mesh.mIndexBase = &(mCurRequest->mIndices[0]);
		mesh.mIndexStrideBytes = 6;
	
		mesh.mNumTriangles = mCurRequest->mIndices.size()/3;
	}

	if ((vertex_based || mesh.mNumTriangles > 0) && mesh.mNumVertices > 2)
	{
		LLCDResult ret = LLCD_OK;
		if (LLConvexDecomposition::getInstance() != NULL)
		{
			ret  = LLConvexDecomposition::getInstance()->setMeshData(&mesh, vertex_based);
		}

		if (ret)
		{
			LL_ERRS(LOG_MESH) << "Convex Decomposition thread valid but could not set mesh data." << LL_ENDL;
		}
	}
}

void LLPhysicsDecomp::doDecomposition()
{
	LLCDMeshData mesh;
	S32 stage = mStageID[mCurRequest->mStage];

	if (LLConvexDecomposition::getInstance() == NULL)
	{
		// stub library. do nothing.
		return;
	}

	//load data intoLLCD
	if (stage == 0)
	{
		setMeshData(mesh, false);
	}
		
	//build parameter map
	std::map<std::string, const LLCDParam*> param_map;

	static const LLCDParam* params = NULL;
	static S32 param_count = 0;
	if (!params)
	{
		param_count = LLConvexDecomposition::getInstance()->getParameters(&params);
	}
	
	for (S32 i = 0; i < param_count; ++i)
	{
		param_map[params[i].mName] = params+i;
	}

	LLCDResult ret = LLCD_OK;
	//set parameter values
	for (decomp_params::iterator iter = mCurRequest->mParams.begin(); iter != mCurRequest->mParams.end(); ++iter)
	{
		const std::string& name = iter->first;
		const LLSD& value = iter->second;

		const LLCDParam* param = param_map[name];

		if (param == NULL)
		{	//couldn't find valid parameter
			continue;
		}


		if (param->mType == LLCDParam::LLCD_FLOAT)
		{
			ret = LLConvexDecomposition::getInstance()->setParam(param->mName, (F32) value.asReal());
		}
		else if (param->mType == LLCDParam::LLCD_INTEGER ||
				 param->mType == LLCDParam::LLCD_ENUM)
		{
			ret = LLConvexDecomposition::getInstance()->setParam(param->mName, value.asInteger());
		}
		else if (param->mType == LLCDParam::LLCD_BOOLEAN)
		{
			ret = LLConvexDecomposition::getInstance()->setParam(param->mName, value.asBoolean());
		}
	}

	mCurRequest->setStatusMessage("Executing.");

	if (LLConvexDecomposition::getInstance() != NULL)
	{
		ret = LLConvexDecomposition::getInstance()->executeStage(stage);
	}

	if (ret)
	{
		LL_WARNS(LOG_MESH) << "Convex Decomposition thread valid but could not execute stage " << stage << "."
						   << LL_ENDL;
		LLMutexLock lock(mMutex);

		mCurRequest->mHull.clear();
		mCurRequest->mHullMesh.clear();

		mCurRequest->setStatusMessage("FAIL");
		
		completeCurrent();
	}
	else
	{
		mCurRequest->setStatusMessage("Reading results");

		S32 num_hulls =0;
		if (LLConvexDecomposition::getInstance() != NULL)
		{
			num_hulls = LLConvexDecomposition::getInstance()->getNumHullsFromStage(stage);
		}
		
		{
			LLMutexLock lock(mMutex);
			mCurRequest->mHull.clear();
			mCurRequest->mHull.resize(num_hulls);

			mCurRequest->mHullMesh.clear();
			mCurRequest->mHullMesh.resize(num_hulls);
		}

		for (S32 i = 0; i < num_hulls; ++i)
		{
			std::vector<LLVector3> p;
			LLCDHull hull;
			// if LLConvexDecomposition is a stub, num_hulls should have been set to 0 above, and we should not reach this code
			LLConvexDecomposition::getInstance()->getHullFromStage(stage, i, &hull);

			const F32* v = hull.mVertexBase;

			for (S32 j = 0; j < hull.mNumVertices; ++j)
			{
				LLVector3 vert(v[0], v[1], v[2]); 
				p.push_back(vert);
				v = (F32*) (((U8*) v) + hull.mVertexStrideBytes);
			}
			
			LLCDMeshData mesh;
			// if LLConvexDecomposition is a stub, num_hulls should have been set to 0 above, and we should not reach this code
			LLConvexDecomposition::getInstance()->getMeshFromStage(stage, i, &mesh);

			get_vertex_buffer_from_mesh(mesh, mCurRequest->mHullMesh[i]);
			
			{
				LLMutexLock lock(mMutex);
				mCurRequest->mHull[i] = p;
			}
		}
	
		{
			LLMutexLock lock(mMutex);
			mCurRequest->setStatusMessage("FAIL");
			completeCurrent();						
		}
	}
}

void LLPhysicsDecomp::completeCurrent()
{
	LLMutexLock lock(mMutex);
	mCompletedQ.push(mCurRequest);
	mCurRequest = NULL;
}

void LLPhysicsDecomp::notifyCompleted()
{
	if (!mCompletedQ.empty())
	{
		decltype(mCompletedQ) completed_q;
		{
			LLMutexLock lock(mMutex);
			completed_q.swap(mCompletedQ);
		}
		while (!completed_q.empty())
		{
			Request* req = completed_q.front();
			req->completed();
			completed_q.pop();
		}
	}
}


void make_box(LLPhysicsDecomp::Request * request)
{
	LLVector3 min,max;
	min = request->mPositions[0];
	max = min;

	for (U32 i = 0; i < request->mPositions.size(); ++i)
	{
		update_min_max(min, max, request->mPositions[i]);
	}

	request->mHull.clear();
	
	LLModel::hull box;
	box.push_back(LLVector3(min[0],min[1],min[2]));
	box.push_back(LLVector3(max[0],min[1],min[2]));
	box.push_back(LLVector3(min[0],max[1],min[2]));
	box.push_back(LLVector3(max[0],max[1],min[2]));
	box.push_back(LLVector3(min[0],min[1],max[2]));
	box.push_back(LLVector3(max[0],min[1],max[2]));
	box.push_back(LLVector3(min[0],max[1],max[2]));
	box.push_back(LLVector3(max[0],max[1],max[2]));

	request->mHull.push_back(box);
}


void LLPhysicsDecomp::doDecompositionSingleHull()
{
	LLConvexDecomposition* decomp = LLConvexDecomposition::getInstance();

	if (decomp == NULL)
	{
		//stub. do nothing.
		return;
	}
	
	LLCDMeshData mesh;	

	setMeshData(mesh, true);

	LLCDResult ret = decomp->buildSingleHull() ;
	if(ret)
	{
		LL_WARNS(LOG_MESH) << "Could not execute decomposition stage when attempting to create single hull." << LL_ENDL;
		make_box(mCurRequest);
	}
	else
	{
		{
			LLMutexLock lock(mMutex);
			mCurRequest->mHull.clear();
			mCurRequest->mHull.resize(1);
			mCurRequest->mHullMesh.clear();
		}

		std::vector<LLVector3> p;
		LLCDHull hull;
		
		// if LLConvexDecomposition is a stub, num_hulls should have been set to 0 above, and we should not reach this code
		decomp->getSingleHull(&hull);

		const F32* v = hull.mVertexBase;

		for (S32 j = 0; j < hull.mNumVertices; ++j)
		{
			LLVector3 vert(v[0], v[1], v[2]); 
			p.push_back(vert);
			v = (F32*) (((U8*) v) + hull.mVertexStrideBytes);
		}

		{
			LLMutexLock lock(mMutex);
			mCurRequest->mHull[0] = p;
		}
	}		

	{
		completeCurrent();
		
	}
}

#ifdef ND_HASCONVEXDECOMP_TRACER

class ndDecompTracer: public ndConvexDecompositionTracer
{
	int mRefCount;

public:
	ndDecompTracer()
		: mRefCount(0)
	{
	}

	virtual ~ndDecompTracer() { }

	virtual void trace( char const *a_strMsg )
	{
		LL_INFOS() << a_strMsg << LL_ENDL;
	}

	virtual void startTraceData( char const *a_strWhat)
	{
		LL_INFOS() << a_strWhat << LL_ENDL;
	}

	virtual void traceData( char const *a_strData )
	{
		LL_INFOS() << a_strData << LL_ENDL;
	}

	virtual void endTraceData()
	{

	}

	virtual int getLevel()
	{
		return eTraceFunctions;// | eTraceData;
	}

	virtual void addref()
	{
		++mRefCount;
	}

	virtual void release()
	{
		--mRefCount;
		if( mRefCount == 0 )
			delete this;
	}
};

#endif

void LLPhysicsDecomp::run()
{
	LLConvexDecomposition* decomp = LLConvexDecomposition::getInstance();
	if (decomp == NULL)
	{
		// stub library. Set init to true so the main thread
		// doesn't wait for this to finish.
		mInited = true;
		return;
	}

#ifdef ND_HASCONVEXDECOMP_TRACER
	ndConvexDecompositionTracable *pTraceable = dynamic_cast< ndConvexDecompositionTracable* >( decomp );

	if( pTraceable )
		pTraceable->setTracer( new ndDecompTracer() );
#endif

	decomp->initThread();
	mInited = true;

	static const LLCDStageData* stages = NULL;
	static S32 num_stages = 0;
	
	if (!stages)
	{
		num_stages = decomp->getStages(&stages);
	}

	for (S32 i = 0; i < num_stages; i++)
	{
		mStageID[stages[i].mName] = i;
	}

	mSignal->lock();

	while (!mQuitting)
	{
		mSignal->wait();
		while (!mQuitting && !mRequestQ.empty())
		{
			{
				LLMutexLock lock(mMutex);
				mCurRequest = mRequestQ.front();
				mRequestQ.pop();
			}

			S32& id = *(mCurRequest->mDecompID);
			if (id == -1)
			{
				decomp->genDecomposition(id);
			}
			decomp->bindDecomposition(id);

			if (mCurRequest->mStage == "single_hull")
			{
				doDecompositionSingleHull();
			}
			else
			{
				doDecomposition();
			}		
		}
	}

	decomp->quitThread();
	
	if (mSignal->isLocked())
	{ //let go of mSignal's associated mutex
		mSignal->unlock();
	}

	mDone = true;
}

void LLPhysicsDecomp::Request::assignData(LLModel* mdl) 
{
	if (!mdl)
	{
		return ;
	}

	U16 index_offset = 0;
	U16 tri[3] ;

	mPositions.clear();
	mIndices.clear();
	mBBox[1] = LLVector3(F32_MIN, F32_MIN, F32_MIN) ;
	mBBox[0] = LLVector3(F32_MAX, F32_MAX, F32_MAX) ;
		
	//queue up vertex positions and indices
	for (S32 i = 0; i < mdl->getNumVolumeFaces(); ++i)
	{
		const LLVolumeFace& face = mdl->getVolumeFace(i);
		if (mPositions.size() + face.mNumVertices > 65535)
		{
			continue;
		}

		for (U32 j = 0; j < (U32)face.mNumVertices; ++j)
		{
			mPositions.push_back(LLVector3(face.mPositions[j].getF32ptr()));
			for(U32 k = 0 ; k < 3 ; k++)
			{
				mBBox[0].mV[k] = llmin(mBBox[0].mV[k], mPositions[j].mV[k]) ;
				mBBox[1].mV[k] = llmax(mBBox[1].mV[k], mPositions[j].mV[k]) ;
			}
		}

		updateTriangleAreaThreshold() ;

		for (U32 j = 0; j+2 < (U32)face.mNumIndices; j += 3)
		{
			tri[0] = face.mIndices[j] + index_offset ;
			tri[1] = face.mIndices[j + 1] + index_offset ;
			tri[2] = face.mIndices[j + 2] + index_offset ;
				
			if(isValidTriangle(tri[0], tri[1], tri[2]))
			{
				mIndices.push_back(tri[0]);
				mIndices.push_back(tri[1]);
				mIndices.push_back(tri[2]);
			}
		}

		index_offset += face.mNumVertices;
	}

	return ;
}

void LLPhysicsDecomp::Request::updateTriangleAreaThreshold() 
{
	F32 range = mBBox[1].mV[0] - mBBox[0].mV[0] ;
	range = llmin(range, mBBox[1].mV[1] - mBBox[0].mV[1]) ;
	range = llmin(range, mBBox[1].mV[2] - mBBox[0].mV[2]) ;

	mTriangleAreaThreshold = llmin(0.0002f, range * 0.000002f) ;
}

//check if the triangle area is large enough to qualify for a valid triangle
bool LLPhysicsDecomp::Request::isValidTriangle(U16 idx1, U16 idx2, U16 idx3) 
{
	LLVector3 a = mPositions[idx2] - mPositions[idx1] ;
	LLVector3 b = mPositions[idx3] - mPositions[idx1] ;
	F32 c = a * b ;

	return ((a*a) * (b*b) - c * c) > mTriangleAreaThreshold ;
}

void LLPhysicsDecomp::Request::setStatusMessage(const std::string& msg)
{
	mStatusMessage = msg;
}

void LLMeshRepository::buildPhysicsMesh(LLModel::Decomposition& decomp)
{
	decomp.mMesh.resize(decomp.mHull.size());

	for (U32 i = 0; i < decomp.mHull.size(); ++i)
	{
		LLCDHull hull;
		hull.mNumVertices = decomp.mHull[i].size();
		hull.mVertexBase = decomp.mHull[i][0].mV;
		hull.mVertexStrideBytes = 12;

		LLCDMeshData mesh;
		LLCDResult res = LLCD_OK;
		if (LLConvexDecomposition::getInstance() != NULL)
		{
			res = LLConvexDecomposition::getInstance()->getMeshFromHull(&hull, &mesh);
		}
		if (res == LLCD_OK)
		{
			get_vertex_buffer_from_mesh(mesh, decomp.mMesh[i]);
		}
	}

	if (!decomp.mBaseHull.empty() && decomp.mBaseHullMesh.empty())
	{	//get mesh for base hull
		LLCDHull hull;
		hull.mNumVertices = decomp.mBaseHull.size();
		hull.mVertexBase = decomp.mBaseHull[0].mV;
		hull.mVertexStrideBytes = 12;

		LLCDMeshData mesh;
		LLCDResult res = LLCD_OK;
		if (LLConvexDecomposition::getInstance() != NULL)
		{
			res = LLConvexDecomposition::getInstance()->getMeshFromHull(&hull, &mesh);
		}
		if (res == LLCD_OK)
		{
			get_vertex_buffer_from_mesh(mesh, decomp.mBaseHullMesh);
		}
	}
}


bool LLMeshRepository::meshUploadEnabled()
{
	LLViewerRegion *region = gAgent.getRegion();
	static const LLCachedControl<bool> mesh_enabled("MeshEnabled");
	if(mesh_enabled &&
	   region)
	{
		return region->meshUploadEnabled();
	}
	return false;
}

bool LLMeshRepository::meshRezEnabled()
{
	LLViewerRegion *region = gAgent.getRegion();
	static const LLCachedControl<bool> mesh_enabled("MeshEnabled");
	if(mesh_enabled && 
	   region)
	{
		return region->meshRezEnabled();
	}
	return false;
}
