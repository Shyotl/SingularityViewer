/** 
 * @file llvertexbuffer.cpp
 * @brief LLVertexBuffer implementation
 *
 * $LicenseInfo:firstyear=2003&license=viewerlgpl$
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

#include "linden_common.h"

#include <boost/static_assert.hpp>
#include "llsys.h"
#include "llvertexbuffer.h"
// #include "llrender.h"
#include "llglheaders.h"
#include "llrender.h"
#include "llvector4a.h"
#include "llshadermgr.h"
#include "llglslshader.h"
#include "llmemory.h"

//#define VBO_USE_DSA

#if LL_MSVC
// disable warning "'A': inherits 'B:fn' via dominance" error regarding pure virtuals. MS won't pull their head out of their ass and fix it.
#pragma warning (disable:4250)
#endif

//Next Highest Power Of Two
//helper function, returns first number > v that is a power of 2, or v if v is already a power of 2
U32 nhpo2(U32 v)
{
	U32 r = 1;
	while (r < v) {
		r *= 2;
	}
	return r;
}

//which power of 2 is i?
//assumes i is a power of 2 > 0
U32 wpo2(U32 i)
{
	llassert(i > 0);
	llassert(nhpo2(i) == i);

	U32 r = 0;

	while (i >>= 1) ++r;

	return r;
}

#include <algorithm>
struct ModifiedMemoryTracker
{
	typedef std::vector<std::pair<U32, U32> > range_array;

	ModifiedMemoryTracker() : /*mIdx(0),*/ mTotalSize(0)
	{
		mRange.reserve(256);
	}
	void clear()
	{
		mRange.clear();
	}
	bool expand(U32 offset, U32 size)
	{
		U32 end = offset + size;
		for (U32 i = 0; i < mRange.size(); ++i)
		{
			if (end >= mRange[i].first && offset <= mRange[i].second)
			{
				mTotalSize -= mRange[i].second - mRange[i].first;
				mRange[i] = std::make_pair(llmin(mRange[i].first, offset), llmax(mRange[i].second, end));
				mTotalSize += mRange[i].second - mRange[i].first;
				return true;
			}
		}
		mRange.push_back(std::make_pair(offset, end));
		return false;
	}
	const range_array& getRanges(U32& len, U32& size)
	{
		mCompressedRange.clear();
		std::sort(mRange.begin(), mRange.end());
		mCompressedRange.push_back(*mRange.begin());
		size = mRange[0].second - mRange[0].first;
		U32 idx = 0;
		for (U32 i = 1; i < mRange.size(); ++i)
		{
			if (mRange[i].first <= mCompressedRange[idx].second)
			{
				size += mRange[i].second - mCompressedRange[idx].second;
				mCompressedRange[idx].second = mRange[i].second;
			}
			else
			{
				++idx;
				size += mRange[i].second - mRange[i].first;
				mCompressedRange.push_back(mRange[i]);
			}
		}
		len = mCompressedRange.size();
		clear();
		return mCompressedRange;
	}
private:
	range_array mRange;
	range_array mCompressedRange;
	U32 mTotalSize;
};

#include "lltimer.h"
#include "llframetimer.h"

namespace GLTimer
{
	enum gl_time_t
	{
		GL_TIME_MAP,
		GL_TIME_UNMAP,
		GL_TIME_BIND,
		GL_TIME_FENCE_WAIT,
		GL_TIME_FENCE_PLACE,
		GL_TIME_ALLOC,
		GL_TIME_PREDRAW,
		GL_TIMER_COUNT
	};
	//U64 sTimers[GL_TIMER_COUNT];
	//F64 sLastTimer;

	struct Timer
	{
		Timer(gl_time_t type)// : mType(type), mTicks(__rdtsc())
		{}
		~Timer()
		{
			/*sTimers[mType] += __rdtsc() - mTicks;
			F32 elapsed = LLFrameTimer::getElapsedSeconds();
			F64 elapsed_delta = elapsed - sLastTimer;
			if (elapsed_delta >= 5.f)
			{
			LL_INFOS() << "GL_TIME_MAP = " << (sTimers[GL_TIME_MAP] / elapsed_delta) * .000001f << "mticks/s" << LL_ENDL;
			LL_INFOS() << "GL_TIME_UNMAP = " << (sTimers[GL_TIME_UNMAP] / elapsed_delta) * .000001f << "mticks/s" << LL_ENDL;
			LL_INFOS() << "GL_TIME_BIND = " << (sTimers[GL_TIME_BIND] / elapsed_delta) * .000001f << "mticks/s" << LL_ENDL;
			LL_INFOS() << "GL_TIME_FENCE_WAIT = " << (sTimers[GL_TIME_FENCE_WAIT] / elapsed_delta) * .000001f << "mticks/s" << LL_ENDL;
			LL_INFOS() << "GL_TIME_FENCE_PLACE = " << (sTimers[GL_TIME_FENCE_PLACE] / elapsed_delta) * .000001f << "mticks/s" << LL_ENDL;
			LL_INFOS() << "GL_TIME_ALLOC = " << (sTimers[GL_TIME_ALLOC] / elapsed_delta) * .000001f << "mticks/s" << LL_ENDL;
			LL_INFOS() << "GL_TIME_PREDRAW = " << (sTimers[GL_TIME_PREDRAW] / elapsed_delta) * .000001f << "mticks/s" << LL_ENDL;
			U64 total = 0;
			for (U32 i = 0; i < GL_TIMER_COUNT; ++i)
			{
			total += sTimers[i];
			sTimers[i] = 0;
			}
			LL_INFOS() << "TOTAL = " << (total / elapsed_delta) * .000001f << "mticks/s" << LL_ENDL;
			sLastTimer = elapsed;
			}*/
		}
		//gl_time_t mType;
		//U64 mTicks;
	};
};

struct GLNullSync
{
	void clear() {}
	void wait() {}
	void place() {}
};

class GLFenceSync
{
public:
	GLFenceSync() : mFence(0)
	{}
	~GLFenceSync()
	{
		clear();
	}
	void clear()
	{
		///New buffer. Can drop sync.
		if (mFence)
			glDeleteSync(mFence);
		mFence = 0;
	}
	void wait()
	{
		if (!mFence)
			return;
		GLTimer::Timer timer(GLTimer::GL_TIME_FENCE_WAIT);
		GLenum ret = 0;
		ret = glClientWaitSync(mFence, 0, 1 << 27);
		llassert(ret != GL_TIMEOUT_EXPIRED && ret != GL_WAIT_FAILED);
		clear();
	}
	void place()
	{
		GLTimer::Timer timer(GLTimer::GL_TIME_FENCE_PLACE);
		if (mFence)
			glDeleteSync(mFence);
		mFence = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);	//This call is INCREDIBLY slow on AMD, making using fencesyncs non-viable.
	}
private:
	GLsync mFence;
};

U32 sBufferTarget[] =
{
	GL_ARRAY_BUFFER_ARB,		//BUFFER_VERTICES
	GL_ELEMENT_ARRAY_BUFFER_ARB	//BUFFER_INDICES
};
static U32 sLastBoundBuffer[LLVertexBuffer::BUFFER_TYPE_MAX] = { 0 };

bool vbo_dsa = false;

// Actual underlying gl buffer impl. The dsa if/else statements are ugly, but are also faster than function pointers or virtual methods. Branch prediction should be fairly accurate.
class GLBuffer
{
private:
	U32 mHandle;
	U8 mType;
public:
	GLBuffer(U32 type) : mHandle(0), mType(type)
	{}
	~GLBuffer()
	{
		release();
	}
	U32 getHandle() const
	{
		return mHandle;
	}
	U32 getTarget() const
	{
		return sBufferTarget[mType];
	}
	void release()
	{
		if (mHandle)
		{
			if (sLastBoundBuffer[mType] == mHandle)
			{
				sLastBoundBuffer[mType] = 0;
			}
			glDeleteBuffersARB(1, &mHandle);
			stop_glerror();
			mHandle = 0;
		}
	}
	void bind()
	{
		if (sLastBoundBuffer[mType] != mHandle)
		{
			glBindBufferARB(sBufferTarget[mType], mHandle);
			sLastBoundBuffer[mType] = mHandle;
			stop_glerror();
		}
	}
	void unbind()
	{
		if (sLastBoundBuffer[mType] == mHandle)
		{
			sLastBoundBuffer[mType] = 0;
			glBindBufferARB(sBufferTarget[mType], 0);
			stop_glerror();
		}
	}
	void create()
	{
		bool was_bound = mHandle && (sLastBoundBuffer[mType] == mHandle);
		release();
		if (!vbo_dsa)
			glGenBuffersARB(1, &mHandle);
		else
			glCreateBuffers(1, &mHandle);
		if (was_bound)
			bind();
	}
	void bufferData(GLsizeiptrARB size, const void *data, GLenum usage)
	{
		if (!vbo_dsa)
			glBufferDataARB(sBufferTarget[mType], size, data, usage);
		else
			glNamedBufferData(mHandle, size, data, usage);
	}
	void bufferSubData(GLintptrARB offset, GLsizeiptrARB size, const void *data)
	{
		if (!vbo_dsa)
			glBufferSubDataARB(sBufferTarget[mType], offset, size, data);
		else
			glNamedBufferSubData(mHandle, offset, size, data);
	}
	U8* map(U32 mask)
	{
		if (!vbo_dsa)
			return (U8*)glMapBufferARB(sBufferTarget[mType], mask);
		else
			return (U8*)glMapNamedBuffer(mHandle, mask);
	}
	U8* mapRange(GLintptrARB offset, GLsizeiptrARB size, U32 mask)
	{
		if (!vbo_dsa)
			return (U8*)glMapBufferRange(sBufferTarget[mType], offset, size, mask);
		else
			return (U8*)glMapNamedBufferRange(mHandle, offset, size, mask);

	}
	void unmap()
	{
		if (!vbo_dsa)
			glUnmapBufferARB(sBufferTarget[mType]);
		else
			glUnmapNamedBuffer(mHandle);
	}
	void bufferStorage(GLsizeiptrARB size, const void* data, U32 mask)
	{
		if (!vbo_dsa)
			glBufferStorage(sBufferTarget[mType], size, data, mask);
		else
			glNamedBufferStorage(mHandle, size, data, mask);
	}
	void flushRange(GLintptrARB offset, GLsizeiptrARB size)
	{
		if (!vbo_dsa)
			glFlushMappedBufferRange(sBufferTarget[mType], offset, size);
		else
			glFlushMappedNamedBufferRange(mHandle, offset, size);
	}
};

class BaseClientAttrBuffer : public virtual IAttrBuffer
{
protected:
	BaseClientAttrBuffer() : IAttrBuffer(), mClientData(NULL), mClientDataSize(0)
	{}
	~BaseClientAttrBuffer()
	{
		ll_aligned_free<64>(mClientData);
	}
	bool setSize(U32 new_size)
	{
		llassert(new_size != 0);
		if (new_size > 0 && (new_size > mClientDataSize || new_size * 2 < mClientDataSize))
		{
			mClientDataSize = new_size;
			ll_aligned_free<64>(mClientData);
			mClientData = (U8*)ll_aligned_malloc<64>(new_size);
			return true;
		}
		return false;
	}
	U8* map(U32 offset, U32 size, bool range_update)
	{
		return mClientData + offset;
	}
	void unmap() {}
	U8* getClientPtr()
	{
		return mClientData;
	}
	U8* getAttribPtr()
	{
		return mClientData;
	}

	U8* mClientData;
	U32 mClientDataSize;
};

template <typename FENCE = GLNullSync>
class BaseGLAttrBuffer : public virtual IAttrBuffer, public FENCE
{
protected:
	typedef FENCE FenceType;
	BaseGLAttrBuffer(U32 type, U32 usage) : IAttrBuffer(), FenceType(), mUsage(usage), mGLBuffer(type), mGLBufferSize(0), mGLBufferAllocatedSize(0)
	{
		mGLBuffer.create();
		++LLVertexBuffer::sGLCount;
	}
	~BaseGLAttrBuffer()
	{
		--LLVertexBuffer::sGLCount;
	}
	bool setSize(U32 new_size)
	{
		llassert(new_size != 0);
		mGLBufferSize = new_size;
		if (new_size > 0 && (new_size > mGLBufferAllocatedSize || new_size * 2 < mGLBufferAllocatedSize))
		{
			FenceType::clear();
			mGLBufferAllocatedSize = new_size;
			mGLBuffer.bufferData(new_size, nullptr, mUsage);
			return true;
		}
		return false;
	}
	bool bind()
	{
		mGLBuffer.bind();
		return true;
	}
	bool bindForEdit()
	{
#ifndef VBO_USE_DSA
		return bind();
#else
		false; //Do nothing if using DSA.
#endif
	}
	void unbind()
	{
		mGLBuffer.unbind();
	}
	void unmap() {}
	void preDraw() {}
	void placeFence()
	{
		FenceType::place();
	}
	U8* getClientPtr()
	{
		return NULL;
	}
	U8* getAttribPtr()
	{
		return NULL;
	}

	GLBuffer mGLBuffer;
	U32 mUsage;
	U32 mGLBufferSize;
	U32 mGLBufferAllocatedSize;
};

template<typename MAP, typename FENCE = GLNullSync>
class BaseMappedAttrBuffer : public BaseGLAttrBuffer<FENCE>
{
protected:
	typedef MAP MapType;
	BaseMappedAttrBuffer(U32 type, U32 usage) : BaseGLAttrBuffer(type, usage), mMappedPtr(0)
	{}
	U8* map(U32 offset, U32 size, bool range_update)
	{
		if (!mMappedPtr)
		{
			FenceType::wait();
			mMappedPtr = toMapType()->mapBuffer(0, mGLBufferSize);
		}
		return mMappedPtr + offset;
	}
	void unmap()
	{
		if (mMappedPtr)
			mGLBuffer.unmap();
		mMappedPtr = NULL;
	}
	U8* mMappedPtr;
	MapType* toMapType() { return static_cast<MapType*>(this); }
};

template<typename MAP, typename FENCE = GLNullSync>
class BaseMappedRangeableAttrBuffer : public BaseMappedAttrBuffer<MAP, FENCE>, ModifiedMemoryTracker
{
protected:
	BaseMappedRangeableAttrBuffer(U32 type, U32 usage) : BaseMappedAttrBuffer(type, usage)
	{}
	U8* map(U32 offset, U32 size, bool range_update)
	{
		expand(offset, size);
		return BaseMappedAttrBuffer::map(offset, size, range_update);
	}
	void unmap()
	{
		U32 len, size = 0;
		const range_array& ranges = getRanges(len, size);
		if (len)
		{
			FenceType::wait();
			for (U32 i = 0; i < len; ++i)
			{
				toMapType()->flushRange(ranges[i].first, ranges[i].second - ranges[i].first);
			}
		}
	}
};

class LegacyClientAttrBuffer : public BaseClientAttrBuffer
{
public:
	LegacyClientAttrBuffer(U32 type) : BaseClientAttrBuffer(), mType(type)
	{}
	bool bind()
	{
		if (sLastBoundBuffer[mType] != 0)
		{
			glBindBufferARB(sBufferTarget[mType], 0);
			sLastBoundBuffer[mType] = 0;
		}
		return true;
	}
	bool bindForEdit() { return false; }
	void unbind() {}
	void preDraw() {}
	void placeFence() {}
private:
	U32 mType;
};

class StandardAttrBuffer : public BaseGLAttrBuffer<>, public BaseClientAttrBuffer
{
public:
	StandardAttrBuffer(U32 type, U32 usage) : BaseGLAttrBuffer<>(type, usage), BaseClientAttrBuffer(), mOrphaned(false)
	{}
	bool setSize(U32 new_size)
	{
		return BaseGLAttrBuffer::setSize(new_size) && BaseClientAttrBuffer::setSize(new_size);
	}
	U8* map(U32 offset, U32 size, bool range_update)
	{
		orphan();
		return BaseClientAttrBuffer::map(offset, size, range_update);
	}
	void unmap()
	{
		orphan();
		mOrphaned = false;
		copySubData(0, mGLBufferSize);
	}
	U8* getClientPtr()
	{
		return BaseClientAttrBuffer::mClientData;
	}
	U8* getAttribPtr()
	{
		return BaseGLAttrBuffer::getAttribPtr();
	}

	void orphan()
	{
		if (!mOrphaned)
			mGLBuffer.bufferData(mGLBufferAllocatedSize, NULL, mUsage);
		mOrphaned = true;
	}
	void copySubData(U32 offset, U32 size)
	{
		mGLBuffer.bufferSubData(offset, size, mClientData + offset);
	}
private:
	bool mOrphaned;
};

class StandardRangeableAttrBuffer : public StandardAttrBuffer, public ModifiedMemoryTracker
{
public:
	StandardRangeableAttrBuffer(U32 type, U32 usage) : StandardAttrBuffer(type, usage), ModifiedMemoryTracker()
	{}
	U8* map(U32 offset, U32 size, bool range_update)
	{
		expand(offset, size);
		return BaseClientAttrBuffer::map(offset, size, range_update);
	}
	void unmap()
	{
		U32 len, size = 0;
		const range_array& ranges = getRanges(len, size);
		if (len)
		{
			if (size == mGLBufferSize)
			{
				StandardAttrBuffer::unmap();
				return;
			}
			for (U32 i = 0; i < len; ++i)
			{
				copySubData(ranges[i].first, ranges[i].second - ranges[i].first);
			}
		}
	}
};

#if GL_AMD_pinned_memory
#define GL_EXTERNAL_VIRTUAL_MEMORY_AMD 37216
class PinnedAttrBuffer : public BaseGLAttrBuffer<GLFenceSync>
{
public:
	PinnedAttrBuffer(U32 type, U32 usage) : BaseGLAttrBuffer(type, usage), mClientData(0), mClientDataSize(0)
	{}
	~PinnedAttrBuffer()
	{
		ll_aligned_free_16(mClientData);
	}
	bool bindForEdit()
	{
		return bind();
	}
	bool setSize(U32 new_size)
	{
		if (new_size > 0 && (new_size > mClientDataSize || new_size * 2 < mClientDataSize))
		{
			FenceType::wait();	//Defer deletion until absolute sure it's safe.
			ll_aligned_free<4096>(mClientData);
			mClientDataSize = new_size;
			mClientData = (U8*)ll_aligned_malloc<4096>(new_size);
			glBindBufferARB(GL_EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD, mGLBuffer.getHandle());
			glBufferDataARB(GL_EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD, new_size, mClientData, mUsage);
			glBindBufferARB(GL_EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD, 0);

			return true;
		}
		return false;
	}
	U8* map(U32 offset, U32 size, bool range_update)
	{
		FenceType::wait();
		return mClientData + offset;
	}
	U8* getClientPtr()
	{
		return mClientData;
	}
private:
	U8* mClientData;
	U32 mClientDataSize;
};
#endif

template <typename FENCE = GLNullSync, int N = 1>
class GLMultiFenceSync
{
public:
	GLMultiFenceSync() : mWriteIdx(0), mLocked(false), mJustLocked(U8_MAX)
	{ }

	void clear()
	{
		for (U32 i = 0; i < N; ++i)
			mSyncs[i].clear();
		mWriteIdx = 0;
		mLocked = false;
		mJustLocked = U8_MAX;
	}
	void wait()
	{
		if (mLocked)
		{
			mWriteIdx = (mWriteIdx + 1) % N;
			mLocked = false;
		}
		for (U32 i = 0; i < N - 1; ++i)
		{
			llassert(mJustLocked != (mWriteIdx + i) % 3);
		}
		mSyncs[mWriteIdx].wait();
	}
	void place()
	{
		if (N > 1)
		{
			if (!mLocked)
			{
				mLocked = true;
				mJustLocked = mWriteIdx;
			}
		}
		mSyncs[mWriteIdx].place();
	}
	U8 getWriteBuff() const
	{
		return mWriteIdx;
	}
	U8 getNumBuffers() const
	{
		return N;
	}
	FENCE mSyncs[N];
	U8 mWriteIdx;
	bool mLocked;
	U8 mJustLocked;
};

class ImmutableBufferStorage : public BaseGLAttrBuffer<>
{
public:
	ImmutableBufferStorage(U32 type, U32 usage, U32 mask) : BaseGLAttrBuffer(type, usage), mMappedData(0), mMask(mask | GL_MAP_WRITE_BIT | GL_MAP_PERSISTENT_BIT | GL_MAP_COHERENT_BIT)
	{}
	~ImmutableBufferStorage()
	{
		if (mMappedData)
			mGLBuffer.unmap();
	}
	bool setSize(U32 new_size)
	{
		mGLBufferSize = new_size;
		if (new_size > 0 && (new_size > mGLBufferAllocatedSize || new_size * 2 < mGLBufferAllocatedSize))
		{
			mGLBufferAllocatedSize = new_size;
			if (mMappedData)
				mGLBuffer.unmap();

			//Immutable means we need to create an entire new vbo.
			mGLBuffer.create();
			mGLBuffer.bufferStorage(mGLBufferAllocatedSize, NULL, mMask);
			mMappedData = mGLBuffer.mapRange(0, mGLBufferAllocatedSize, mMask & ~(GL_CLIENT_STORAGE_BIT | GL_DYNAMIC_STORAGE_BIT));
			return true;
		}
		return false;
	}
	U8* map(U32 offset, U32 size, bool range_update)
	{
		return mMappedData + offset;
	}
public:
	U8* mMappedData;
	const U32 mMask;
};

class ImmutableBufferStorage2 : public virtual ImmutableBufferStorage, public virtual BaseClientAttrBuffer
{
public:
	ImmutableBufferStorage2(U32 type, U32 usage, U32 mask) : ImmutableBufferStorage(type, usage, mask), BaseClientAttrBuffer()
	{}
	bool setSize(U32 new_size)
	{
		return BaseClientAttrBuffer::setSize(new_size) && ImmutableBufferStorage::setSize(new_size);
	}
	U8* map(U32 offset, U32 size, bool range_update)
	{
		return getClientPtr() + offset;
	}
	void unmap()
	{
		FenceType::wait();
		LLVector4a::memcpyNonAliased16((F32*)mMappedData, (F32*)getClientPtr(), mGLBufferSize);
	}
	U8* getClientPtr()
	{
		return BaseClientAttrBuffer::getClientPtr();
	}
	U8* getAttribPtr()
	{
		return ImmutableBufferStorage::getAttribPtr();
	}
};

struct MappedLegacyAttrBuffer : public BaseMappedAttrBuffer < MappedLegacyAttrBuffer >
{
	MappedLegacyAttrBuffer(U32 type, U32 usage) : BaseMappedAttrBuffer(type, usage)
	{}
	U8* mapBuffer(U32 offset, U32 size)
	{
		return mGLBuffer.map(GL_WRITE_ONLY);
	}
};

#ifdef GL_ARB_map_buffer_range
struct MappedAttrBuffer : public BaseMappedAttrBuffer < MappedAttrBuffer >
{
	MappedAttrBuffer(U32 type, U32 usage) : BaseMappedAttrBuffer(type, usage)
	{}
	U8* mapBuffer(U32 offset, U32 size)
	{
		return mGLBuffer.mapRange(offset, size, GL_MAP_WRITE_BIT);
	}
};

struct MappedRangeableAttrBuffer : public BaseMappedRangeableAttrBuffer < MappedRangeableAttrBuffer >
{
	MappedRangeableAttrBuffer(U32 type, U32 usage) : BaseMappedRangeableAttrBuffer(type, usage)
	{}
	U8* mapBuffer(U32 offset, U32 size)
	{
		return mGLBuffer.mapRange(offset, size, GL_MAP_WRITE_BIT | GL_MAP_FLUSH_EXPLICIT_BIT);
	}
	void flushRange(U32 offset, U32 size)
	{
		mGLBuffer.flushRange(offset, size);
	}
};
#endif

#ifdef GL_APPLE_flush_buffer_range
struct MappedRangeableAPPLEAttrBuffer : public BaseMappedRangeableAttrBuffer < MappedRangeableAttrBuffer >
{
	MappedRangeableAPPLEAttrBuffer(U32 type, U32 usage) : BaseMappedRangeableAttrBuffer(type, usage)
	{}
	bool bindForEdit()
	{
		return bind();
	}
	U8* mapBuffer(U32 offset, U32 size)
	{
		glBufferParameteriAPPLE(GL_ARRAY_BUFFER_ARB, GL_BUFFER_SERIALIZED_MODIFY_APPLE, GL_FALSE);
		glBufferParameteriAPPLE(GL_ARRAY_BUFFER_ARB, GL_BUFFER_FLUSHING_UNMAP_APPLE, GL_FALSE);
		return (U8*)glMapBufferARB(mGLBuffer.getTarget(), GL_WRITE_ONLY);
	}
	void flushRange(U32 offset, U32 size)
	{
		glFlushMappedBufferRangeAPPLE(mGLBuffer.getTarget(), offset, size);
	}
};
#endif

#if 0
class VAO
{
public:
	VAO(bool emulated) : mVAO(emulated ? 0 : LLVertexBuffer::getVAOName()), mCachedState(emulated ? sSharedState : mLocalState)
	{
		for (U32 i = 0; i < LLVertexBuffer::BUFFER_TYPE_MAX; ++i)
			mBuffers[i] = NULL;
	}
	~VAO()
	{
		unbind();
		if (!isEmulatedVAO())
			LLVertexBuffer::releaseVAOName(mVAO);
	}
	void bind()
	{
		if (sBoundVAO != this)
		{
			LL_INFOS() << "sBoundVAO = " << (U32) this << LL_ENDL;
			sBoundVAO = this;
			if (!isEmulatedVAO())
				glBindVertexArray(mVAO);
		}
		bindVBO(LLVertexBuffer::BUFFER_VERTICES);
		bindVBO(LLVertexBuffer::BUFFER_INDICES);
	}
	void setBuffer(U32 type, IAttrBuffer* buffer)
	{
		if (sBoundVAO == this)
		{
			LL_INFOS() << "setting " << type << " " << (U32)buffer << LL_ENDL;
			mBuffers[type] = buffer;
			bind();
		}
		else
			mBuffers[type] = buffer;
	}
	void updateAttributes(LLVertexBuffer* buffer, U32 count, U32 mask)
	{
		llassert_always(sBoundVAO == this);
		llassert_always(mCachedState.mBound[LLVertexBuffer::BUFFER_VERTICES] == mBuffers[LLVertexBuffer::BUFFER_VERTICES]);
		if (!isEmulatedVAO())
		{
			mask = buffer->getTypeMask();
		}
		if (count != mCachedState.mElemCount || (mask & ~mCachedState.mTypeMask))
		{
			buffer->setupVertexBuffer(mask);
		}
		if (mask & ~mCachedState.mTypeMask || (!LLGLSLShader::sNoFixedFunction && mask != mCachedState.mTypeMask))
		{
			buffer->setupClientArrays(mask, true);
		}
		mCachedState.mElemCount = count;
		mCachedState.mTypeMask = mask;
	}
private:
	bool isEmulatedVAO() const
	{
		return !mVAO;
	}
	void bindVBO(U32 type)
	{

		if (mCachedState.mBound[type] != mBuffers[type] && mBuffers[type])
			//if (mBuffers[type])
		{
			LL_INFOS() << "binding " << type << " " << (U32)mBuffers[type] << LL_ENDL;
			mBuffers[type]->bind();
		}
		mCachedState.mBound[type] = mBuffers[type];
		LLVertexBuffer::sGLBoundBuffer[type] = mBuffers[type];
	}
	void unbindVBO(U32 type)
	{
		if (mCachedState.mBound[type] == mBuffers[type] && mBuffers[type])
			//if (mBuffers[type])
		{
			mBuffers[type]->unbind();
		}
		mCachedState.mBound[type] = NULL;
		LLVertexBuffer::sGLBoundBuffer[type] = NULL;
	}
	void unbind()
	{
		if (sBoundVAO == this)
		{
			unbindVBO(LLVertexBuffer::BUFFER_VERTICES);
			unbindVBO(LLVertexBuffer::BUFFER_INDICES);
			glBindVertexArray(0);
			sBoundVAO = NULL;
		}
	}


	LLVertexBuffer* mVBO;
	U32 mVAO;
	IAttrBuffer* mBuffers[LLVertexBuffer::BUFFER_TYPE_MAX];

	struct cached_state_t
	{
		cached_state_t() : mElemCount(0), mTypeMask(0)
		{
			for (U32 i = 0; i < LLVertexBuffer::BUFFER_TYPE_MAX; ++i)
				mBound[i] = NULL;
		}
		U32 mElemCount;
		U32 mTypeMask;
		IAttrBuffer* mBound[LLVertexBuffer::BUFFER_TYPE_MAX];
	};
	cached_state_t& mCachedState;

	cached_state_t mLocalState;			//If using a real vao mCachedState is assigned to this.

	static cached_state_t sSharedState;	//If not using real vao mCachedState is assigned to this.
	static VAO* sBoundVAO;
};
VAO::cached_state_t VAO::sSharedState;
VAO* VAO::sBoundVAO = NULL;
#endif

//============================================================================

//static
std::list<U32> LLVertexBuffer::sAvailableVAOName;
U32 LLVertexBuffer::sCurVAOName = 1;

U32 LLVertexBuffer::sAllocatedBytes[LLVertexBuffer::BUFFER_TYPE_MAX] = { 0 };
U32 LLVertexBuffer::sElementCount[LLVertexBuffer::BUFFER_TYPE_MAX] = { 0 };

U32 LLVertexBuffer::sBindCount = 0;
U32 LLVertexBuffer::sSetCount = 0;
S32 LLVertexBuffer::sCount = 0;
S32 LLVertexBuffer::sGLCount = 0;
S32 LLVertexBuffer::sMappedCount = 0;
bool LLVertexBuffer::sDisableVBOMapping = true;	//Temporary workaround for vbo mapping being straight up broken
bool LLVertexBuffer::sEnableVBOs = true;
IAttrBuffer* LLVertexBuffer::sGLBoundBuffer[LLVertexBuffer::BUFFER_TYPE_MAX] = { 0 };
U32 LLVertexBuffer::sGLRenderArray = 0;
U32 LLVertexBuffer::sLastMask = 0;
U8* LLVertexBuffer::sLastOffset = 0;
bool LLVertexBuffer::sUseStreamDraw = true;
bool LLVertexBuffer::sUseVAO = false;
bool LLVertexBuffer::sPreferStreamDraw = false;
bool LLVertexBuffer::sLastNoFixedFunction = false;
LLVertexBuffer* LLVertexBuffer::sUtilityBuffer = nullptr;

LLVertexBuffer::attrib_prop_t LLVertexBuffer::sAttribProp[TYPE_INDEX + 1] =
{
	{ "TYPE_VERTEX", 3, sizeof(LLVector4), GL_FLOAT, false, false },
	{ "TYPE_NORMAL", 3, sizeof(LLVector4), GL_FLOAT, false, false },
	{ "TYPE_TEXCOORD0", 2, sizeof(LLVector2), GL_FLOAT, false, false },
	{ "TYPE_TEXCOORD1", 2, sizeof(LLVector2), GL_FLOAT, false, false },
	{ "TYPE_TEXCOORD2", 2, sizeof(LLVector2), GL_FLOAT, false, false },
	{ "TYPE_TEXCOORD3", 2, sizeof(LLVector2), GL_FLOAT, false, false },
	{ "TYPE_COLOR", 4, sizeof(LLColor4U), GL_UNSIGNED_BYTE, false, true },
	{ "TYPE_EMISSIVE", 4, sizeof(LLColor4U), GL_UNSIGNED_BYTE, false, true },
	{ "TYPE_TANGENT", 4, sizeof(LLVector4), GL_FLOAT, false, false },
	{ "TYPE_WEIGHT", 1, sizeof(F32), GL_FLOAT, false, false },
	{ "TYPE_WEIGHT4", 4, sizeof(LLVector4), GL_FLOAT, false, false },
	{ "TYPE_CLOTHWEIGHT", 4, sizeof(LLVector4), GL_FLOAT, false, true },
	{ "TYPE_TEXTURE_INDEX", 4, sizeof(LLVector4), GL_UNSIGNED_BYTE, true, false }, // (actually exists as position.w), no extra data, but stride is 16 bytes
	{ "TYPE_MAX", 0, 0, 0, false, false },
	{ "TYPE_INDEX", 1, sizeof(U16), GL_UNSIGNED_BYTE, true, false }
};

// static
const std::string& LLVertexBuffer::getTypeName(U8 i)
{
	llassert_always(i < (U8)(sizeof(sAttribProp) / sizeof(sAttribProp[0])));
	return sAttribProp[i].mName;
}

U32 LLVertexBuffer::sGLMode[LLRender::NUM_MODES] =
{
	GL_TRIANGLES,
	GL_TRIANGLE_STRIP,
	GL_TRIANGLE_FAN,
	GL_POINTS,
	GL_LINES,
	GL_LINE_STRIP,
	GL_LINE_LOOP,
};

//static
U32 LLVertexBuffer::getVAOName()
{
	U32 ret = 0;

	if (!sAvailableVAOName.empty())
	{
		ret = sAvailableVAOName.front();
		sAvailableVAOName.pop_front();
	}
	else
	{
#ifdef GL_ARB_vertex_array_object
		glGenVertexArrays(1, &ret);
#endif
	}

	return ret;
}

//static
void LLVertexBuffer::releaseVAOName(U32 name)
{
	sAvailableVAOName.push_back(name);
}

//static
void LLVertexBuffer::setupClientArrays(U32 data_mask, bool for_vao_setup)
{
	if (for_vao_setup || (sLastMask != data_mask))
	{
		if (gGLManager.mGLSLVersionMajor < 1 ||
			(gGLManager.mGLSLVersionMajor == 1 && gGLManager.mGLSLVersionMinor <= 20))
		{
			//make sure texture index is disabled
			data_mask = data_mask & ~MAP_TEXTURE_INDEX;
		}

		if (sLastNoFixedFunction)
		{
			for (U32 i = 0; i < TYPE_MAX; ++i)
			{
				U32 mask = 1 << i;

				if (for_vao_setup || ((sLastMask & mask) != (data_mask & mask)))
				{
					if (!(data_mask & mask)) //needs to be disabled
						glDisableVertexAttribArrayARB(i);
					else //needs to be enabled
						glEnableVertexAttribArrayARB(i);
				}
			}
		}
		else
		{
			struct { U32 arr, type, tex; }
			attrib[] =
			{
				{ GL_VERTEX_ARRAY, TYPE_VERTEX, 0 },
				{ GL_NORMAL_ARRAY, TYPE_NORMAL, 0 },
				{ GL_TEXTURE_COORD_ARRAY, TYPE_TEXCOORD0, GL_TEXTURE0_ARB },
				{ GL_TEXTURE_COORD_ARRAY, TYPE_TEXCOORD1, GL_TEXTURE1_ARB },
				{ GL_TEXTURE_COORD_ARRAY, TYPE_TEXCOORD2, GL_TEXTURE2_ARB },
				{ GL_TEXTURE_COORD_ARRAY, TYPE_TEXCOORD3, GL_TEXTURE3_ARB },
				{ GL_COLOR_ARRAY, TYPE_COLOR, 0 },
				{ GL_TEXTURE_COORD_ARRAY, TYPE_TANGENT, GL_TEXTURE2_ARB }
			};

			for (U32 i = 0; i < sizeof(attrib) / sizeof(attrib[0]); ++i)
			{
				U32 mask = 1 << attrib[i].type;
				if (for_vao_setup || ((sLastMask & mask) != (data_mask & mask)))
				{
					if (attrib[i].arr == GL_TEXTURE_COORD_ARRAY)
						glClientActiveTextureARB(attrib[i].tex);
					if (!(data_mask & mask)) //needs to be disabled
					{
						glDisableClientState(attrib[i].arr);
					}
					else //needs to be enabled
					{
						glEnableClientState(attrib[i].arr);
					}
					if (attrib[i].arr == GL_TEXTURE_COORD_ARRAY)
						glClientActiveTextureARB(GL_TEXTURE0_ARB);
				}
				if (gDebugGL)
				{
					if (attrib[i].arr == GL_TEXTURE_COORD_ARRAY)
						glClientActiveTextureARB(attrib[i].tex);
					if (bool(data_mask & mask) != bool(glIsEnabled(attrib[i].arr)))
					{
						if (gDebugSession)
							gFailLog << "Bad client state! " << sAttribProp[attrib[i].type].mName << ((data_mask & mask) ? " disabled." : " enabled.") << std::endl;
						else
							LL_ERRS() << "Bad client state! " << sAttribProp[attrib[i].type].mName << ((data_mask & mask) ? " disabled." : " enabled.") << LL_ENDL;
					}
					if (attrib[i].arr == GL_TEXTURE_COORD_ARRAY)
						glClientActiveTextureARB(GL_TEXTURE0_ARB);
				}
			}
		}
		if (!for_vao_setup)
			sLastMask = data_mask;
	}
}

//static
void LLVertexBuffer::drawArrays(U32 mode, const std::vector<LLVector3>& pos, const std::vector<LLVector3>& norm)
{
	llassert(!LLGLSLShader::sNoFixedFunction || LLGLSLShader::sCurBoundShaderPtr != NULL);
	gGL.syncMatrices();

	U32 count = pos.size();

	llassert(norm.size() >= pos.size());
	llassert(count > 0);

	if (count == 0)
	{
		LL_WARNS() << "Called drawArrays with 0 vertices" << LL_ENDL;
		return;
	}

	if (norm.size() < pos.size())
	{
		LL_WARNS() << "Called drawArrays with #" << norm.size() << " normals and #" << pos.size() << " vertices" << LL_ENDL;
		return;
	}

	if (!sUtilityBuffer)
	{
		sUtilityBuffer = new LL_VERTEXBUFFER(MAP_VERTEX | MAP_NORMAL | MAP_TEXCOORD0, GL_STREAM_DRAW);
	}
	if (sUtilityBuffer->getNumVerts() < (S32)count)
	{
		sUtilityBuffer->resizeBuffer(count, count);
	}

	LLStrider<LLVector3> vertex_strider;
	LLStrider<LLVector3> normal_strider;
	sUtilityBuffer->getVertexStrider(vertex_strider);
	sUtilityBuffer->getNormalStrider(normal_strider);
	for (U32 i = 0; i < count; ++i)
	{
		*(vertex_strider++) = pos[i];
		*(normal_strider++) = norm[i];
	}

	sUtilityBuffer->setBuffer(MAP_VERTEX | MAP_NORMAL);
	sUtilityBuffer->drawArrays(mode, 0, pos.size());
}

//static
void LLVertexBuffer::drawElements(U32 mode, const S32 num_vertices, const LLVector4a* pos, const LLVector2* tc, S32 num_indices, const U16* indicesp)
{
	llassert(!LLGLSLShader::sNoFixedFunction || LLGLSLShader::sCurBoundShaderPtr != NULL);

	if (num_vertices <= 0)
	{
		LL_WARNS() << "Called drawElements with 0 vertices" << LL_ENDL;
		return;
	}

	if (num_indices <= 0)
	{
		LL_WARNS() << "Called drawElements with 0 indices" << LL_ENDL;
		return;
	}

	gGL.syncMatrices();

	if (!sUtilityBuffer)
	{
		sUtilityBuffer = new LL_VERTEXBUFFER(MAP_VERTEX | MAP_NORMAL | MAP_TEXCOORD0, GL_STREAM_DRAW);
	}
	if (sUtilityBuffer->getNumVerts() < num_vertices || sUtilityBuffer->getNumIndices() < num_indices)
	{
		sUtilityBuffer->resizeBuffer(llmax(sUtilityBuffer->getNumVerts(), num_vertices), llmax(sUtilityBuffer->getNumIndices(), num_indices));
	}

	U32 mask = LLVertexBuffer::MAP_VERTEX;

	LLStrider<U16> index_strider;
	LLStrider<LLVector4a> vertex_strider;
	sUtilityBuffer->getIndexStrider(index_strider);
	sUtilityBuffer->getVertexStrider(vertex_strider);
	LLVector4a::memcpyNonAliased16((F32*)index_strider.get(), (F32*)indicesp, LLVertexBuffer::calcTypeBlockSize(LLVertexBuffer::TYPE_INDEX, num_indices));
	LLVector4a::memcpyNonAliased16((F32*)vertex_strider.get(), (F32*)pos, LLVertexBuffer::calcTypeBlockSize(LLVertexBuffer::TYPE_VERTEX, num_vertices));
	if (tc)
	{
		mask = mask | LLVertexBuffer::MAP_TEXCOORD0;
		LLStrider<LLVector2> tc_strider;
		sUtilityBuffer->getTexCoord0Strider(tc_strider);
		LLVector4a::memcpyNonAliased16((F32*)tc_strider.get(), (F32*)tc, LLVertexBuffer::calcTypeBlockSize(LLVertexBuffer::TYPE_TEXCOORD0, num_vertices));
	}

	sUtilityBuffer->setBuffer(mask);
	sUtilityBuffer->draw(mode, num_indices, 0);
}

void LLVertexBuffer::validateRange(U32 start, U32 end, U32 count, U32 indices_offset, U32 mode) const
{
	llassert(!LLGLSLShader::sNoFixedFunction || LLGLSLShader::sCurBoundShaderPtr != NULL);
	llassert(mNumIndices >= 0);
	if (start >= (U32)mBuffer[BUFFER_VERTICES].mElementCount ||
		end >= (U32)mBuffer[BUFFER_VERTICES].mElementCount)
	{
		LL_ERRS() << "Bad vertex buffer draw range: [" << start << ", " << end << "] vs " << mBuffer[BUFFER_VERTICES].mElementCount << LL_ENDL;
	}

	if (mBuffer[BUFFER_INDICES].mImpl && (indices_offset >= (U32)mBuffer[BUFFER_INDICES].mElementCount ||
		indices_offset + count > (U32)mBuffer[BUFFER_INDICES].mElementCount))
	{
		LL_ERRS() << "Bad index buffer draw range: [" << indices_offset << ", " << indices_offset + count << "] vs " << mBuffer[BUFFER_INDICES].mElementCount << LL_ENDL;
	}

	if (mGLArray)
	{
		if (mGLArray != sGLRenderArray)
			LL_ERRS() << "Wrong vertex array bound." << LL_ENDL;
	}
	else
	{
		/*if (sGLBoundBuffer[BUFFER_VERTICES] != mBuffer[BUFFER_VERTICES].mImpl)
		LL_ERRS() << "Wrong VBO bound." << LL_ENDL;

		if (mBuffer[BUFFER_INDICES].mImpl && sGLBoundBuffer[BUFFER_VERTICES] != mBuffer[BUFFER_VERTICES].mImpl)
		LL_ERRS() << "Wrong VBO bound." << LL_ENDL;*/
	}

	if (mode >= LLRender::NUM_MODES)
		LL_ERRS() << "Invalid draw mode: " << mode << LL_ENDL;

	if (gDebugGL)
	{
		if (mBuffer[BUFFER_INDICES].mImpl && mBuffer[BUFFER_INDICES].mImpl->getClientPtr())
		{
			U16* idx = ((U16*)mBuffer[BUFFER_INDICES].mImpl->getClientPtr()) + indices_offset;
			for (U32 i = 0; i < count; ++i)
			{
				if (idx[i] < start || idx[i] > end)
				{
					LL_ERRS() << "Index out of range: " << idx[i] << " not in [" << start << ", " << end << "]" << LL_ENDL;
				}
			}
		}

		LLGLSLShader* shader = LLGLSLShader::sCurBoundShaderPtr;

		if (shader && shader->mFeatures.mIndexedTextureChannels > 1 && mBuffer[BUFFER_VERTICES].mImpl && mBuffer[BUFFER_VERTICES].mImpl->getClientPtr())
		{
			LLVector4a* v = (LLVector4a*)(mBuffer[BUFFER_VERTICES].mImpl->getClientPtr() + mOffsets[TYPE_VERTEX]);

			for (U32 i = start; i <= end; i++)
			{
				S32 idx = (S32)(v[i][3] + 0.25f);
				if (idx < 0 || idx >= S32(shader->mFeatures.mIndexedTextureChannels))
				{
					LL_ERRS() << "Bad texture index found in vertex data stream." << LL_ENDL;
				}
			}
		}
	}
}

void LLVertexBuffer::drawRange(U32 mode, U32 start, U32 end, U32 count, U32 indices_offset) const
{
	*const_cast<U32*>(&drawn_frame) = LLFrameTimer::getFrameCount();
	llassert(!mNeedsSetup);
	bindBuffer(mBuffer[BUFFER_INDICES], true);
	bindBuffer(mBuffer[BUFFER_VERTICES], true);

	validateRange(start, end, count, indices_offset, mode);
	gGL.syncMatrices();

	{
		GLTimer::Timer timer(GLTimer::GL_TIME_PREDRAW);
		mBuffer[BUFFER_VERTICES].mImpl->preDraw();
		mBuffer[BUFFER_INDICES].mImpl->preDraw();
	}

	stop_glerror();
	glDrawRangeElements(sGLMode[mode], start, end, count, GL_UNSIGNED_SHORT,
		((U16*)getIndicesAttribPointer()) + indices_offset);
	stop_glerror();

	mBuffer[BUFFER_VERTICES].mImpl->placeFence();
	mBuffer[BUFFER_INDICES].mImpl->placeFence();
}

void LLVertexBuffer::draw(U32 mode, U32 count, U32 indices_offset) const
{
	*const_cast<U32*>(&drawn_frame) = LLFrameTimer::getFrameCount();
	llassert(!mNeedsSetup);
	bindBuffer(mBuffer[BUFFER_INDICES], true);
	bindBuffer(mBuffer[BUFFER_VERTICES], true);

	validateRange(0, mBuffer[BUFFER_VERTICES].mElementCount - 1, count, indices_offset, mode);
	gGL.syncMatrices();

	{
		GLTimer::Timer timer(GLTimer::GL_TIME_PREDRAW);
		mBuffer[BUFFER_VERTICES].mImpl->preDraw();
		mBuffer[BUFFER_INDICES].mImpl->preDraw();
	}

	stop_glerror();
	glDrawElements(sGLMode[mode], count, GL_UNSIGNED_SHORT,
		((U16*)getIndicesAttribPointer()) + indices_offset);
	stop_glerror();

	mBuffer[BUFFER_VERTICES].mImpl->placeFence();
	mBuffer[BUFFER_INDICES].mImpl->placeFence();
}

static LLTrace::BlockTimerStatHandle FTM_GL_DRAW_ARRAYS("GL draw arrays");
void LLVertexBuffer::drawArrays(U32 mode, U32 first, U32 count) const
{
	*const_cast<U32*>(&drawn_frame) = LLFrameTimer::getFrameCount();
	llassert(!mNeedsSetup);
	bindBuffer(mBuffer[BUFFER_VERTICES], true);

	validateRange(first, first + count - 1, 0, 0, mode);
	gGL.syncMatrices();

	{
		GLTimer::Timer timer(GLTimer::GL_TIME_PREDRAW);
		mBuffer[BUFFER_VERTICES].mImpl->preDraw();
	}

	//LLFastTimer t2(FTM_GL_DRAW_ARRAYS);
	stop_glerror();
	glDrawArrays(sGLMode[mode], first, count);
	stop_glerror();

	mBuffer[BUFFER_VERTICES].mImpl->placeFence();
}

//static
void LLVertexBuffer::initClass(bool use_vbo, bool no_vbo_mapping)
{
	sEnableVBOs = use_vbo && gGLManager.mHasVertexBufferObject;
	sDisableVBOMapping = no_vbo_mapping;
	sLastNoFixedFunction = LLGLSLShader::sNoFixedFunction;
}

//static 
void LLVertexBuffer::unbind()
{
	for (U32 i = 0; i < BUFFER_TYPE_MAX; ++i)
	{
		if (sGLBoundBuffer[i])
		{
			sGLBoundBuffer[i]->unbind();
			sGLBoundBuffer[i] = NULL;
		}
	}
	setupClientArrays(0);
}

//static
void LLVertexBuffer::cleanupClass()
{
	unbind();
	delete sUtilityBuffer;
	sUtilityBuffer = nullptr;
}

//----------------------------------------------------------------------------

S32 LLVertexBuffer::determineUsage(S32 usage)
{
	S32 ret_usage = usage;

	if (!sEnableVBOs)
	{
		ret_usage = 0;
	}

	if (ret_usage == GL_STREAM_DRAW_ARB && !sUseStreamDraw)
	{
		ret_usage = 0;
	}

	if (ret_usage == GL_DYNAMIC_DRAW_ARB && sPreferStreamDraw)
	{
		ret_usage = GL_STREAM_DRAW_ARB;
	}

	if (ret_usage == 0 && LLRender::sGLCoreProfile)
	{ //MUST use VBOs for all rendering
		ret_usage = GL_STREAM_DRAW_ARB;
	}

	/*if (ret_usage && ret_usage != GL_STREAM_DRAW_ARB)
	{ //only stream_draw and dynamic_draw are supported when using VBOs, dynamic draw is the default
	if (sDisableVBOMapping)
	{ //always use stream draw if VBO mapping is disabled
	ret_usage = GL_STREAM_DRAW_ARB;
	}
	else
	{
	ret_usage = GL_DYNAMIC_DRAW_ARB;
	}
	}*/

	return ret_usage;
}

std::map<LLVertexBuffer*, const char*> sVboList;

extern U32 sRebuildNum;

LLVertexBuffer::LLVertexBuffer(U32 typemask, S32 usage, const char* caller) :
	LLRefCount(),
	mTypeMask(typemask),
	mUsage(LLVertexBuffer::determineUsage(usage)),
	mGLArray(0),
	mNeedsSetup(true),
	drawn_frame(U32_MAX)
{
	sVboList.insert(std::make_pair(this, caller));
	mRebuiltIDX = sRebuildNum;
#if !LL_DARWIN
	if (mTypeMask & MAP_TEXTURE_INDEX && gGLManager.mGLSLVersionMajor < 2 && gGLManager.mGLSLVersionMinor < 30)
#endif
		mTypeMask &= ~MAP_TEXTURE_INDEX;

	//zero out offsets
	memset(mOffsets, 0, sizeof(mOffsets));

	for (U32 i = 0; i < BUFFER_TYPE_MAX; ++i)
		mBuffer[i].mType = (buffer_type_t)i;

	sCount++;
}

//static
S32 LLVertexBuffer::calcOffsets(const U32& typemask, S32* offsets, S32 num_vertices)
{
	S32 offset = 0;
	for (S32 i = 0; i<TYPE_TEXTURE_INDEX; i++)
	{
		U32 mask = 1 << i;
		if (typemask & mask)
		{
			if (offsets && sAttribProp[i].mDataSize)
			{
				offsets[i] = offset;
				offset += calcTypeBlockSize(i, num_vertices);	//All blocks are 16byte aligned.
			}
		}
	}

	offsets[TYPE_TEXTURE_INDEX] = offsets[TYPE_VERTEX] + 12;
	offsets[TYPE_MAX] = offsets[TYPE_INDEX] = 0;

	return offset + 16;
}

//static 
S32 LLVertexBuffer::calcVertexSize(const U32& typemask)
{
	S32 size = 0;
	for (S32 i = 0; i < TYPE_TEXTURE_INDEX; i++)
	{
		U32 mask = 1 << i;
		if (typemask & mask)
		{
			size += sAttribProp[i].mDataSize;
		}
	}

	return size;
}

// protected, use unref()
//virtual
LLVertexBuffer::~LLVertexBuffer()
{
	sVboList.erase(this);
	for (U32 i = 0; i < BUFFER_TYPE_MAX; ++i)
	{
		releaseBuffer(mBuffer[i]);
	}

	if (mGLArray)
	{
#if GL_ARB_vertex_array_object
		releaseVAOName(mGLArray);
#endif
	}

	sCount--;
};
//----------------------------------------------------------------------------
LLVector3 vbo_bits;
LLVector3 vbo_conv;


void LLVertexBuffer::releaseBuffer(buffer_data_t& buffer)
{
	//mParentVAO->setBuffer(buffer.mType, NULL);
	if (buffer.mImpl && sGLBoundBuffer[buffer.mType] == buffer.mImpl)
	{
		buffer.mImpl->unbind();
		sGLBoundBuffer[buffer.mType] = NULL;
	}

	sElementCount[buffer.mType] -= buffer.mElementCount;
	sAllocatedBytes[buffer.mType] -= buffer.mSize;

	delete buffer.mImpl;
	buffer.mImpl = NULL;
	buffer.mSize = 0;
	buffer.mElementCount = 0;
	buffer.mIsLocked = false;
}

IAttrBuffer* LLVertexBuffer::createImplBuffer(buffer_data_t& buffer, U32 size)
{
	if (size == 0)
	{
		return NULL;
	}

	if (!useVBOs())
		return new LegacyClientAttrBuffer(buffer.mType);
	/*else if (mUsage == GL_DYNAMIC_DRAW)
	return new StandardRangeableAttrBuffer(buffer.mType, mUsage);
	else
	return new StandardAttrBuffer(buffer.mType, mUsage);*/

	U32 mUsages[3] = {
		GL_STREAM_DRAW,
		GL_DYNAMIC_DRAW,
		GL_STATIC_DRAW
	};

	for (U32 i = 0; i < 3; ++i)
	{
		if (mUsages[i] == mUsage)
		{
			U32 usage = mUsage;
			switch ((int)vbo_conv.mV[i])
			{
			case 0: break;
			case 1: usage = GL_STREAM_DRAW; break;
			case 2: usage = GL_DYNAMIC_DRAW; break;
			case 3: usage = GL_STATIC_DRAW; break;
			default:;
			}

			switch ((int)vbo_bits.mV[i])
			{
			case 0: if (!LLRender::sGLCoreProfile) return new LegacyClientAttrBuffer(buffer.mType);
			case 1: return new StandardAttrBuffer(buffer.mType, usage);
			case 2: return new StandardRangeableAttrBuffer(buffer.mType, usage);
			case 3: return new MappedAttrBuffer(buffer.mType, usage);
			case 4: return new MappedRangeableAttrBuffer(buffer.mType, usage);
			case 5: return new MappedRangeableAPPLEAttrBuffer(buffer.mType, usage);
			case 6: return new MappedLegacyAttrBuffer(buffer.mType, usage);
			case 7: return new PinnedAttrBuffer(buffer.mType, usage);
			case 8: return new ImmutableBufferStorage(buffer.mType, usage, 0);
			case 9: return new ImmutableBufferStorage(buffer.mType, usage, GL_DYNAMIC_STORAGE_BIT);
			case 10: return new ImmutableBufferStorage(buffer.mType, usage, GL_CLIENT_STORAGE_BIT);
			case 11: return new ImmutableBufferStorage(buffer.mType, usage, GL_DYNAMIC_STORAGE_BIT | GL_CLIENT_STORAGE_BIT);
			case 12: return new ImmutableBufferStorage2(buffer.mType, usage, 0);
			case 13: return new ImmutableBufferStorage2(buffer.mType, usage, GL_DYNAMIC_STORAGE_BIT);
			case 14: return new ImmutableBufferStorage2(buffer.mType, usage, GL_CLIENT_STORAGE_BIT);
			case 15: return new ImmutableBufferStorage2(buffer.mType, usage, GL_DYNAMIC_STORAGE_BIT | GL_CLIENT_STORAGE_BIT);
			default:
				return new StandardAttrBuffer(buffer.mType, usage);
			}
		}
	}
	LL_ERRS() << "No buffer..?" << LL_ENDL;
	return NULL;
}

bool LLVertexBuffer::updateNumElements(buffer_data_t& buffer, S32 elements)
{
	if (buffer.mType == BUFFER_VERTICES)
	{
		llassert(elements > 0);
	}
	else
	{
		llassert(elements >= 0);
	}

	if (buffer.mElementCount == elements)
		return false;

	stop_glerror();

	if (!elements)
	{
		releaseBuffer(buffer);
		return false;
	}
	else
	{
		U32 needed_size = (buffer.mType == BUFFER_VERTICES) ? ((U32)calcOffsets(mTypeMask, mOffsets, elements)) : (((sizeof(U16) * (U32)elements) + 0xF) & ~0xF);

		if (!buffer.mImpl)
		{
			if (!(buffer.mImpl = createImplBuffer(buffer, needed_size)))
				return false;
			//mParentVAO->setBuffer(buffer.mType, buffer.mImpl);
		}

		sElementCount[buffer.mType] -= buffer.mElementCount;
		sAllocatedBytes[buffer.mType] -= buffer.mSize;
		bindBuffer(buffer, false);
		llassert(needed_size == ((needed_size + 0xF) & ~0xF));
		{
			GLTimer::Timer timer(GLTimer::GL_TIME_ALLOC);
			buffer.mImpl->setSize(needed_size);
		}
		buffer.mElementCount = elements;
		buffer.mSize = needed_size;
		sElementCount[buffer.mType] += elements;
		sAllocatedBytes[buffer.mType] += needed_size;
		sGLBoundBuffer[buffer.mType] = NULL;

		return true;
	}
}

void LLVertexBuffer::allocateBuffer(S32 nverts, S32 nindices, bool new_buffer)
{
	stop_glerror();

	if (nverts < 0 || nindices < 0 ||
		nverts > 65536)
	{
		LL_WARNS() << "Bad vertex buffer allocation: " << nverts << " : " << nindices << LL_ENDL;
	}

	/*if (!mGLArray && useVBOs())
	{
	mGLArray = getVAOName();
	LL_INFOS() << "New vao " << mGLArray << LL_ENDL;
	}*/

	updateNumElements(mBuffer[BUFFER_INDICES], nindices);

	if (updateNumElements(mBuffer[BUFFER_VERTICES], nverts))
	{
		mNeedsSetup = true;
	}
}

static LLTrace::BlockTimerStatHandle FTM_SETUP_VERTEX_ARRAY("Setup VAO");

void LLVertexBuffer::setupVertexArray()
{
	/*stop_glerror();

	LLFastTimer t(FTM_SETUP_VERTEX_ARRAY);
	U32 last_array = sGLRenderArray;
	sGLRenderArray = mGLArray;
	glBindVertexArray(mGLArray);
	LL_INFOS() << "Bind vao " << mGLArray << LL_ENDL;

	mBuffer[BUFFER_VERTICES].mImpl->bind();
	if (mBuffer[BUFFER_INDICES].mImpl)
	mBuffer[BUFFER_INDICES].mImpl->bind();

	setupClientArrays(mTypeMask, true);
	setupVertexBuffer(mTypeMask);
	glBindVertexArray(last_array);
	LL_INFOS() << "Bind vao " << sGLRenderArray << LL_ENDL;*/
}

void LLVertexBuffer::resizeBuffer(S32 newnverts, S32 newnindices)
{
	allocateBuffer(newnverts, newnindices);
}

bool LLVertexBuffer::useVBOs() const
{
	//it's generally ineffective to use VBO for things that are streaming on apple
	return (mUsage != 0);
}

volatile U8* LLVertexBuffer::getIndicesClientPointer() const { return mBuffer[BUFFER_INDICES].mImpl ? mBuffer[BUFFER_INDICES].mImpl->getClientPtr() : NULL; }
volatile U8* LLVertexBuffer::getVerticesClientPointer() const { return mBuffer[BUFFER_VERTICES].mImpl ? mBuffer[BUFFER_VERTICES].mImpl->getClientPtr() : NULL; }
volatile U8* LLVertexBuffer::getIndicesAttribPointer() const { return mBuffer[BUFFER_INDICES].mImpl ? mBuffer[BUFFER_INDICES].mImpl->getAttribPtr() : NULL; }
volatile U8* LLVertexBuffer::getVerticesAttribPointer() const { return mBuffer[BUFFER_VERTICES].mImpl ? mBuffer[BUFFER_VERTICES].mImpl->getAttribPtr() : NULL; }

//----------------------------------------------------------------------------

static LLTrace::BlockTimerStatHandle FTM_VBO_MAP_BUFFER_RANGE("VBO Map Range");
static LLTrace::BlockTimerStatHandle FTM_VBO_MAP_BUFFER("VBO Map");

static LLTrace::BlockTimerStatHandle FTM_VBO_MAP_INDEX_RANGE("IBO Map Range");
static LLTrace::BlockTimerStatHandle FTM_VBO_MAP_INDEX("IBO Map");

// Map for data access
volatile U8* LLVertexBuffer::mapBuffer(buffer_type_t buffer_type, S32 type, S32 index, S32 count, bool map_range)
{
	//llassert_always(drawn_frame != LLFrameTimer::getFrameCount());
	buffer_data_t& buffer = mBuffer[buffer_type];
	if (!buffer.mIsLocked)
		++sMappedCount;
	buffer.mIsLocked = true;

	if (count == -1)
		count = buffer.mElementCount - index;
	llassert(count > 0);

	U32 offset = mOffsets[type] + sAttribProp[type].mDataSize * index;
	U32 length = sAttribProp[type].mDataSize * count;

	if (index >= buffer.mElementCount ||
		(index + count - 1) >= buffer.mElementCount)
	{
		LL_ERRS() << "Bad vertex buffer draw range: [" << index << ", " << (index + count - 1) << "] vs " << buffer.mElementCount << LL_ENDL;
	}

	bindBuffer(buffer, false);
	GLTimer::Timer timer(GLTimer::GL_TIME_MAP);
	return buffer.mImpl->map(offset, length, map_range);
}

static LLTrace::BlockTimerStatHandle FTM_VBO_UNMAP("VBO Unmap");
static LLTrace::BlockTimerStatHandle FTM_VBO_FLUSH_RANGE("Flush VBO Range");

static LLTrace::BlockTimerStatHandle FTM_IBO_UNMAP("IBO Unmap");
static LLTrace::BlockTimerStatHandle FTM_IBO_FLUSH_RANGE("Flush IBO Range");

void LLVertexBuffer::unmapBuffer()
{
	for (U32 i = 0; i < BUFFER_TYPE_MAX; ++i)
	{
		if (mBuffer[i].mIsLocked)
		{
			bindBuffer(mBuffer[i], false);
			if (mBuffer[i].mImpl)
			{
				GLTimer::Timer timer(GLTimer::GL_TIME_UNMAP);
				mBuffer[i].mImpl->unmap();
			}
			mBuffer[i].mIsLocked = false;
			--sMappedCount;
		}
	}

}

//----------------------------------------------------------------------------

template <class T, S32 type>
struct VertexBufferStrider
{
	typedef LLStrider<T> strider_t;
	static bool get(LLVertexBuffer& vbo,
		strider_t& strider,
		S32 index, S32 count, bool map_range)
	{
		if (vbo.hasDataType(type))
		{
			S32 stride = LLVertexBuffer::sAttribProp[type].mDataSize;

			volatile U8* ptr = vbo.mapBuffer(LLVertexBuffer::BUFFER_VERTICES, type, index, count, map_range);

			if (ptr == nullptr)
			{
				LL_WARNS() << "mapVertexBuffer failed!" << LL_ENDL;
				return false;
			}

			strider = (T*)ptr;
			strider.setStride(stride);
			return true;
		}
		else
		{
			LL_WARNS() << "VertexBufferStrider could not find valid vertex data." << LL_ENDL;
		}
		return false;
	}
};

template<class T>
struct VertexBufferStrider<T, LLVertexBuffer::TYPE_INDEX>
{
	typedef LLStrider<T> strider_t;
	static bool get(LLVertexBuffer& vbo,
		strider_t& strider,
		S32 index, S32 count, bool map_range)
	{
		volatile U8* ptr = vbo.mapBuffer(LLVertexBuffer::BUFFER_INDICES, LLVertexBuffer::TYPE_INDEX, index, count, map_range);

		if (ptr == nullptr)
		{
			LL_WARNS() << "mapIndexBuffer failed!" << LL_ENDL;
			return false;
		}

		strider = (T*)ptr;
		strider.setStride(0);
		return true;
	}
};

bool LLVertexBuffer::getVertexStrider(LLStrider<LLVector3>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLVector3, TYPE_VERTEX>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getVertexStrider(LLStrider<LLVector4a>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLVector4a, TYPE_VERTEX>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getIndexStrider(LLStrider<U16>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<U16, TYPE_INDEX>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getTexCoord0Strider(LLStrider<LLVector2>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLVector2, TYPE_TEXCOORD0>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getTexCoord1Strider(LLStrider<LLVector2>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLVector2, TYPE_TEXCOORD1>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getTexCoord2Strider(LLStrider<LLVector2>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLVector2, TYPE_TEXCOORD2>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getNormalStrider(LLStrider<LLVector3>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLVector3, TYPE_NORMAL>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getNormalStrider(LLStrider<LLVector4a>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLVector4a, TYPE_NORMAL>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getTangentStrider(LLStrider<LLVector3>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLVector3, TYPE_TANGENT>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getTangentStrider(LLStrider<LLVector4a>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLVector4a, TYPE_TANGENT>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getColorStrider(LLStrider<LLColor4U>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLColor4U, TYPE_COLOR>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getEmissiveStrider(LLStrider<LLColor4U>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLColor4U, TYPE_EMISSIVE>::get(*this, strider, index, count, map_range);
}
bool LLVertexBuffer::getWeightStrider(LLStrider<F32>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<F32, TYPE_WEIGHT>::get(*this, strider, index, count, map_range);
}

bool LLVertexBuffer::getWeight4Strider(LLStrider<LLVector4a>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLVector4a, TYPE_WEIGHT4>::get(*this, strider, index, count, map_range);
}

bool LLVertexBuffer::getClothWeightStrider(LLStrider<LLVector4a>& strider, S32 index, S32 count, bool map_range)
{
	return VertexBufferStrider<LLVector4a, TYPE_CLOTHWEIGHT>::get(*this, strider, index, count, map_range);
}

//----------------------------------------------------------------------------

static LLTrace::BlockTimerStatHandle FTM_BIND_GL_ARRAY("Bind Array");
bool LLVertexBuffer::bindGLArray() const
{
	/*if (sGLRenderArray != mGLArray)
	{
	sGLRenderArray = mGLArray;
	if (mGLArray)
	{
	LLFastTimer t(FTM_BIND_GL_ARRAY);
	glBindVertexArray(mGLArray);
	LL_INFOS() << "Bind vao " << mGLArray << LL_ENDL;
	stop_glerror();

	//really shouldn't be necessary, but some (INTEL) drivers don't properly restore the
	//state of GL_ELEMENT_ARRAY_BUFFER_BINDING
	if (mBuffer[BUFFER_INDICES].mImpl)
	mBuffer[BUFFER_INDICES].mImpl->bind();
	//bindBuffer(mBuffer[BUFFER_INDICES]);
	return true;
	}
	else
	{
	glBindVertexArray(0);
	LL_INFOS() << "Bind vao " << 0 << LL_ENDL;
	}
	}*/
	return mGLArray;
}

bool LLVertexBuffer::bindBuffer(const buffer_data_t& buffer, bool for_draw, bool force_bind) const
{
	//llassert_always(mParentVAO);
	//mParentVAO->bind(/*buffer.mType*/);

	if (!buffer.mImpl)
		return false;

	if (bindGLArray())
		return false;

	if (sGLBoundBuffer[buffer.mType] != buffer.mImpl)
	{
		stop_glerror();
		GLTimer::Timer timer(GLTimer::GL_TIME_BIND);
		if (for_draw ? buffer.mImpl->bind() : buffer.mImpl->bindForEdit())
		{
			sGLBoundBuffer[buffer.mType] = buffer.mImpl;
			sBindCount++;
		}
		return true;
	}
	return true;
}


void LLVertexBuffer::flush()
{
	stop_glerror();
	unmapBuffer();
	stop_glerror();
}

// Set for rendering
void LLVertexBuffer::setBuffer(U32 data_mask)
{
#if !LL_DARWIN
	if (data_mask & MAP_TEXTURE_INDEX && gGLManager.mGLSLVersionMajor < 2 && gGLManager.mGLSLVersionMinor < 30)
#endif
		data_mask &= ~MAP_TEXTURE_INDEX;
	flush();

	//set up pointers if the data mask is different ...
	/*bool setup = (sLastMask != data_mask);

	if (gDebugGL && data_mask != 0)
	{ //make sure data requirements are fulfilled
	LLGLSLShader* shader = LLGLSLShader::sCurBoundShaderPtr;
	if (shader)
	{
	U32 required_mask = 0;
	for (U32 i = 0; i < LLVertexBuffer::TYPE_TEXTURE_INDEX; ++i)
	{
	if (shader->getAttribLocation(i) > -1)
	{
	U32 required = 1 << i;
	if ((data_mask & required) == 0)
	{
	LL_WARNS() << "Missing attribute: " << LLShaderMgr::instance()->mReservedAttribs[i] << LL_ENDL;
	}

	required_mask |= required;
	}
	}

	if ((data_mask & required_mask) != required_mask)
	{
	LL_ERRS() << "Shader consumption mismatches data provision." << LL_ENDL;
	}
	}
	}*/

	if (bindGLArray())
	{
		if (mNeedsSetup)
			setupVertexArray();
	}
	else //do NOT perform pointer setup if using VAO
	{
		bindBuffer(mBuffer[BUFFER_INDICES], false);
		bool new_vertices = bindBuffer(mBuffer[BUFFER_VERTICES], true);

		if (mNeedsSetup || new_vertices || (data_mask & ~sLastMask) || (sLastOffset != getVerticesAttribPointer()))	//Either new vertices bound, or we have just added more attribs
		{
			sLastOffset = (U8*)getVerticesAttribPointer();
			setupVertexBuffer(data_mask);
		}
		setupClientArrays(data_mask);
	}
	/*mParentVAO->bind(BUFFER_INDICES);
	//mParentVAO->bind(BUFFER_VERTICES);
	mParentVAO->updateAttributes(this, mBuffer[BUFFER_VERTICES].mElementCount, data_mask);*/
}

// virtual (default)
void LLVertexBuffer::setupVertexBuffer(U32 data_mask)
{
	if (!data_mask)
		return;

	mNeedsSetup = false;

	sSetCount++;

	//stop_glerror();
	volatile U8* base = mBuffer[BUFFER_VERTICES].mImpl->getAttribPtr();

	if (gDebugGL && ((data_mask & mTypeMask) != data_mask))
	{
		for (U32 i = 0; i < LLVertexBuffer::TYPE_MAX; ++i)
		{
			U32 mask = 1 << i;
			if (mask & data_mask && !(mask & mTypeMask))
			{ //bit set in data_mask, but not set in mTypeMask
				LL_WARNS() << "Missing required component " << getTypeName(i) << LL_ENDL;
			}
		}
		LL_ERRS() << "LLVertexBuffer::setupVertexBuffer missing required components for supplied data mask." << LL_ENDL;
	}

	if (sLastNoFixedFunction)
	{
		for (U32 i = 0; i < TYPE_MAX; ++i)
		{
			if (data_mask & (1 << i))
			{
				attrib_prop_t& prop = sAttribProp[i];
				if (!prop.mIsInteger)
					glVertexAttribPointerARB(i, prop.mNumComp, prop.mCompType, prop.mIsNormalized, prop.mDataSize, (void*)(base + mOffsets[i]));
				else
					glVertexAttribIPointer(i, prop.mNumComp, prop.mCompType, prop.mDataSize, (void*)(base + mOffsets[i]));
			}
		}
		if (/*(data_mask & MAP_COLOR) &&*/ (data_mask & MAP_EMISSIVE) && hasDataType(TYPE_COLOR))
		{
			//bind emissive instead of color pointer if emissive is present
			S32 loc = TYPE_COLOR;
			attrib_prop_t& prop = sAttribProp[loc];
			glVertexAttribPointerARB(loc, prop.mNumComp, prop.mCompType, prop.mIsNormalized, prop.mDataSize, (void*)(base + mOffsets[TYPE_EMISSIVE]));
		}
		//map emissive to color channel when color is not also being bound to avoid unnecessary shader swaps
		/*else if ((data_mask & MAP_EMISSIVE) && !(mTypeMask & MAP_COLOR) && hasDataType(TYPE_COLOR))
		{
			S32 loc = TYPE_COLOR;
			attrib_prop_t& prop = sAttribProp[loc];
			glVertexAttribPointerARB(loc, prop.mNumComp, prop.mCompType, prop.mIsNormalized, prop.mDataSize, (void*)(base + mOffsets[TYPE_EMISSIVE]));
		}*/
	}
	else
	{
		for (U32 i = 0; i < TYPE_MAX; ++i)
		{
			if (data_mask & (1 << i))
			{
				attrib_prop_t& prop = sAttribProp[i];
				switch (i)
				{
				case TYPE_TANGENT:
				case TYPE_TEXCOORD0:
				case TYPE_TEXCOORD1:
				case TYPE_TEXCOORD2:
				case TYPE_TEXCOORD3:
					glClientActiveTextureARB((i == TYPE_TANGENT) ? GL_TEXTURE2_ARB : (GL_TEXTURE0_ARB + i - TYPE_TEXCOORD0));
					glTexCoordPointer(prop.mNumComp, prop.mCompType, prop.mDataSize, (void*)(base + mOffsets[i]));
					glClientActiveTextureARB(GL_TEXTURE0_ARB);
					break;
				case TYPE_NORMAL:
					glNormalPointer(prop.mCompType, prop.mDataSize, (void*)(base + mOffsets[i]));
					break;
				case TYPE_COLOR:
					glColorPointer(prop.mNumComp, prop.mCompType, prop.mDataSize, (void*)(base + mOffsets[i]));
					break;
				case TYPE_VERTEX:
					glVertexPointer(prop.mNumComp, prop.mCompType, prop.mDataSize, (void*)(base + mOffsets[i]));
				default: break;
				}
			}
		}
	}

	llglassertok();
}
