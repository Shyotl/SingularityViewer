/** 
 * @file llvertexbuffer.h
 * @brief LLVertexBuffer wrapper for OpengGL vertex buffer objects
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

#ifndef LL_LLVERTEXBUFFER_H
#define LL_LLVERTEXBUFFER_H

#include "llgl.h"
#include "v2math.h"
#include "v3math.h"
#include "v4math.h"
#include "v4coloru.h"
#include "llstrider.h"
#include "llrender.h"
#include <set>
#include <vector>
#include <list>

#define LL_MAX_VERTEX_ATTRIB_LOCATION 64

#define LL_VERTEXBUFFER(mask, usage) LLVertexBuffer(mask, usage, __FUNCTION__);

//============================================================================
// NOTES
// Threading:
//  All constructors should take an 'create' paramater which should only be
//  'true' when called from the main thread. Otherwise createGLBuffer() will
//  be called as soon as getVertexPointer(), etc is called (which MUST ONLY be
//  called from the main (i.e OpenGL) thread)


//============================================================================
// abstract class 
class IAttrBuffer
{
public:
	virtual ~IAttrBuffer() {};
	virtual bool setSize(U32 new_size) = 0;

	virtual bool bind() = 0;
	virtual bool bindForEdit() = 0;
	virtual void unbind() = 0;
	virtual U8* map(U32 offset, U32 size, bool range_update) = 0;
	virtual void unmap() = 0;
	virtual U8* getClientPtr() = 0;
	virtual U8* getAttribPtr() = 0;
	virtual void preDraw() = 0;
	virtual void placeFence() = 0;
};

//============================================================================
// base class 
class LLVertexBuffer : public LLRefCount
{
	friend class VAO;
public:
	LLVertexBuffer(const LLVertexBuffer& rhs)
		: mUsage(rhs.mUsage), drawn_frame(U32_MAX)
	{
		*this = rhs;
	}

	const LLVertexBuffer& operator=(const LLVertexBuffer& rhs)
	{
		LL_ERRS() << "Illegal operation!" << LL_ENDL;
		return *this;
	}

	static const std::string& getTypeName(U8 i);

	static std::list<U32> sAvailableVAOName;
	static U32 sCurVAOName;

	static bool	sUseStreamDraw;
	static bool sUseVAO;
	static bool	sPreferStreamDraw;

	static U32 getVAOName();
	static void releaseVAOName(U32 name);

	static void initClass(bool use_vbo, bool no_vbo_mapping);
	static void cleanupClass();
	static void setupClientArrays(U32 data_mask, bool for_vao_setup = false);
	static void drawArrays(U32 mode, const std::vector<LLVector3>& pos, const std::vector<LLVector3>& norm);
	static void drawElements(U32 mode, const S32 num_vertices, const LLVector4a* pos, const LLVector2* tc, S32 num_indices, const U16* indicesp);

 	static void unbind(); //unbind any bound vertex buffer

	//get the size of a vertex with the given typemask
	static S32 calcVertexSize(const U32& typemask);

	//get the size of a buffer with the given typemask and vertex count
	//fill offsets with the offset of each vertex component array into the buffer
	// indexed by the following enum
	static S32 calcOffsets(const U32& typemask, S32* offsets, S32 num_vertices);		

	static U32 calcTypeBlockSize(const U32& type, const U32& count) { return ((count * sAttribProp[type].mDataSize) + 0xF) & ~0xF; }

	//WARNING -- when updating these enums you MUST 
	// 1 - update LLVertexBuffer::sTypeSize
	// 2 - add a strider accessor
	// 3 - modify LLVertexBuffer::setupVertexBuffer
	// 4 - modify LLVertexBuffer::setupClientArray
	// 5 - modify LLViewerShaderMgr::mReservedAttribs
	// 6 - update LLVertexBuffer::setupVertexArray
	enum {
		TYPE_VERTEX = 0,
		TYPE_NORMAL,
		TYPE_TEXCOORD0,
		TYPE_TEXCOORD1,
		TYPE_TEXCOORD2,
		TYPE_TEXCOORD3,
		TYPE_COLOR,
		TYPE_EMISSIVE,
		TYPE_TANGENT,
		TYPE_WEIGHT,
		TYPE_WEIGHT4,
		TYPE_CLOTHWEIGHT,
		TYPE_TEXTURE_INDEX,
		TYPE_MAX,
		TYPE_INDEX,		
	};
	enum {
		MAP_VERTEX = (1<<TYPE_VERTEX),
		MAP_NORMAL = (1<<TYPE_NORMAL),
		MAP_TEXCOORD0 = (1<<TYPE_TEXCOORD0),
		MAP_TEXCOORD1 = (1<<TYPE_TEXCOORD1),
		MAP_TEXCOORD2 = (1<<TYPE_TEXCOORD2),
		MAP_TEXCOORD3 = (1<<TYPE_TEXCOORD3),
		MAP_COLOR = (1<<TYPE_COLOR),
		MAP_EMISSIVE = (1<<TYPE_EMISSIVE),
		// These use VertexAttribPointer and should possibly be made generic
		MAP_TANGENT = (1<<TYPE_TANGENT),
		MAP_WEIGHT = (1<<TYPE_WEIGHT),
		MAP_WEIGHT4 = (1<<TYPE_WEIGHT4),
		MAP_CLOTHWEIGHT = (1<<TYPE_CLOTHWEIGHT),
		MAP_TEXTURE_INDEX = (1<<TYPE_TEXTURE_INDEX),
	};
	enum buffer_type_t
	{
		BUFFER_VERTICES,
		BUFFER_INDICES,
		BUFFER_TYPE_MAX
	};
	
protected:

	struct buffer_data_t
	{
		buffer_data_t() : mImpl(0), mSize(0), mElementCount(0), mIsLocked(false), mType(BUFFER_TYPE_MAX) {}
		IAttrBuffer* mImpl;
		S32 mSize;
		S32 mElementCount;		// Number of elements allocated
		bool mIsLocked : 1;		// if true, vertex buffer is being or has been written to in client memory
		buffer_type_t mType;
	};

	friend class LLRender;

	virtual ~LLVertexBuffer(); // use unref()

	virtual void setupVertexBuffer(U32 data_mask); // pure virtual, called from mapBuffer()
	void setupVertexArray();
	
	bool	bindBuffer(const buffer_data_t& buffer, bool for_draw, bool force_bind = false) const;
	bool	bindGLArray() const;
	void	releaseBuffer(buffer_data_t& buffer);
	IAttrBuffer*	createImplBuffer(buffer_data_t& buffer, U32 size);
	bool	updateNumElements(buffer_data_t& buffer, S32 elements);
	void	unmapBuffer();
		
public:

	LLVertexBuffer(U32 typemask, S32 usage, const char* caller);
	
	// map for data access
	volatile U8*		mapBuffer(buffer_type_t buffer_type, S32 type, S32 index, S32 count, bool map_range);

	// set for rendering
	virtual void	setBuffer(U32 data_mask); 	// calls  setupVertexBuffer() if data_mask is not 0
	void flush(); //flush pending data to GL memory
	// allocate buffer
	void	allocateBuffer(S32 nverts, S32 nindices, bool new_buffer = false);
	virtual void resizeBuffer(S32 newnverts, S32 newnindices);
			
	// Only call each getVertexPointer, etc, once before calling unmapBuffer()
	// call unmapBuffer() after calls to getXXXStrider() before any cals to setBuffer()
	// example:
	//   vb->getVertexBuffer(verts);
	//   vb->getNormalStrider(norms);
	//   setVertsNorms(verts, norms);
	//   vb->unmapBuffer();
	bool getVertexStrider(LLStrider<LLVector3>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getVertexStrider(LLStrider<LLVector4a>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getIndexStrider(LLStrider<U16>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getTexCoord0Strider(LLStrider<LLVector2>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getTexCoord1Strider(LLStrider<LLVector2>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getTexCoord2Strider(LLStrider<LLVector2>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getNormalStrider(LLStrider<LLVector3>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getNormalStrider(LLStrider<LLVector4a>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getTangentStrider(LLStrider<LLVector3>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getTangentStrider(LLStrider<LLVector4a>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getColorStrider(LLStrider<LLColor4U>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getEmissiveStrider(LLStrider<LLColor4U>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getWeightStrider(LLStrider<F32>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getWeight4Strider(LLStrider<LLVector4a>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	bool getClothWeightStrider(LLStrider<LLVector4a>& strider, S32 index=0, S32 count = -1, bool map_range = false);
	

	bool useVBOs() const;
	bool isLocked() const					{ for (U32 i = 0; i < BUFFER_TYPE_MAX; ++i)if (mBuffer[i].mIsLocked) return true; return false; }
	S32 getNumVerts() const					{ return mBuffer[BUFFER_VERTICES].mElementCount; }
	S32 getNumIndices() const				{ return mBuffer[BUFFER_INDICES].mElementCount; }
	
	volatile U8* getIndicesClientPointer() const;
	volatile U8* getVerticesClientPointer() const;
	volatile U8* getIndicesAttribPointer() const;
	volatile U8* getVerticesAttribPointer() const;

	U32 getTypeMask() const					{ return mTypeMask; }
	bool hasDataType(S32 type) const		{ return ((1 << type) & getTypeMask()); }
	S32 getSize() const						{ return mBuffer[BUFFER_VERTICES].mSize; }
	S32 getIndicesSize() const				{ return mBuffer[BUFFER_INDICES].mSize; }
	S32 getOffset(S32 type) const			{ return mOffsets[type]; }
	S32 getUsage() const					{ return mUsage; }
	bool isWriteable() const				{ return mUsage == GL_STREAM_DRAW_ARB; }

	void draw(U32 mode, U32 count, U32 indices_offset) const;
	void drawArrays(U32 mode, U32 offset, U32 count) const;
	void drawRange(U32 mode, U32 start, U32 end, U32 count, U32 indices_offset) const;

	//for debugging, validate data in given range is valid
	void validateRange(U32 start, U32 end, U32 count, U32 offset, U32 mode = 0) const;

	buffer_data_t	mBuffer[BUFFER_TYPE_MAX];
protected:	
	U32			mTypeMask;
	const S32	mUsage;		// GL usage
	U32			mGLArray;	// GL VAO handle
	S32			mOffsets[TYPE_INDEX + 1];
	bool			mNeedsSetup;
	U32				mRebuiltIDX;
	VAO*			mParentVAO;

	static S32 determineUsage(S32 usage);

private:
	U32 drawn_frame;

public:
	static S32 sCount;
	static S32 sGLCount;
	static S32 sMappedCount;
		
	static bool sDisableVBOMapping; //disable glMapBufferARB
	static bool sEnableVBOs;

	static struct attrib_prop_t
	{
		std::string mName;
		U8 mNumComp;			//Number of components actually used
								//NOTE: each component must be AT LEAST 4 bytes in size to avoid a performance penalty on AMD hardware
		U32 mDataSize;			//Real datatype size
		U32 mCompType;			//GL type of each component
		U32 mIsInteger : 1;		//If integer, use glVertexAttribPointer
		U32 mIsNormalized : 1;	//If not an integer, determines if normalized or not.
	}sAttribProp[TYPE_INDEX + 1];
	static U32 sGLMode[LLRender::NUM_MODES];
	static IAttrBuffer* sGLBoundBuffer[BUFFER_TYPE_MAX];
	static U32 sGLRenderArray;
	static U32 sLastMask;
	static U8* sLastOffset;
	static bool sLastNoFixedFunction;
	static U32 sAllocatedBytes[BUFFER_TYPE_MAX];
	static U32 sElementCount[BUFFER_TYPE_MAX];
	static U32 sBindCount;
	static U32 sSetCount;

private:
	static LLVertexBuffer* sUtilityBuffer;
};


#endif // LL_LLVERTEXBUFFER_H
