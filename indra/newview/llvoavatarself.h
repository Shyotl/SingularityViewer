/**
 * @file llvoavatarself.h
 * @brief Declaration of LLVOAvatar class which is a derivation fo
 * LLViewerObject
 *
 * $LicenseInfo:firstyear=2001&license=viewerlgpl$
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

#ifndef LL_LLVOAVATARSELF_H
#define LL_LLVOAVATARSELF_H

#include "llviewertexture.h"
#include "llvoavatar.h"


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// LLVOAvatarSelf
//
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class LLVOAvatarSelf :
	public LLVOAvatar
{

/********************************************************************************
 **                                                                            **
 **                    INITIALIZATION
 **/

public:
	LLVOAvatarSelf(const LLUUID &id, const LLPCode pcode, LLViewerRegion *regionp);
	virtual 				~LLVOAvatarSelf();
	virtual void			markDead();
	virtual void 			initInstance(); // Called after construction to initialize the class.
	void					cleanup();
protected:
	/*virtual*/ BOOL		loadAvatar();
	BOOL					loadAvatarSelf();
	BOOL					buildSkeletonSelf(const LLVOAvatarSkeletonInfo *info);
	BOOL					buildMenus();
	/*virtual*/ BOOL		loadLayersets();

/**                    Initialization
 **                                                                            **
 *******************************************************************************/

/********************************************************************************
 **                                                                            **
 **                    INHERITED
 **/

	//--------------------------------------------------------------------
	// LLViewerObject interface and related
	//--------------------------------------------------------------------
public:
	/*virtual*/ void 		updateRegion(LLViewerRegion *regionp);
	/*virtual*/ BOOL   	 	idleUpdate(LLAgent &agent, LLWorld &world, const F64 &time);

	//--------------------------------------------------------------------
	// LLCharacter interface and related
	//--------------------------------------------------------------------
public:
	/*virtual*/ void 		stopMotionFromSource(const LLUUID& source_id);
	/*virtual*/ void 		requestStopMotion(LLMotion* motion);
	/*virtual*/ LLJoint*	getJoint(const std::string &name);
	
				void		resetJointPositions( void );
	
	/*virtual*/ BOOL setVisualParamWeight(LLVisualParam *which_param, F32 weight, BOOL upload_bake = FALSE );
	/*virtual*/ BOOL setVisualParamWeight(const char* param_name, F32 weight, BOOL upload_bake = FALSE );
	/*virtual*/ BOOL setVisualParamWeight(S32 index, F32 weight, BOOL upload_bake = FALSE );
	/*virtual*/ void updateVisualParams();
	/*virtual*/ void idleUpdateAppearanceAnimation();

private:
	// helper function. Passed in param is assumed to be in avatar's parameter list.
	BOOL setParamWeight(LLViewerVisualParam *param, F32 weight, BOOL upload_bake = FALSE );


/**                    Initialization
 **                                                                            **
 *******************************************************************************/

/********************************************************************************
 **                                                                            **
 **                    STATE
 **/

public:
	/*virtual*/ bool 	isSelf() const { return true; }

	//--------------------------------------------------------------------
	// Updates
	//--------------------------------------------------------------------
public:
	/*virtual*/ BOOL 	updateCharacter(LLAgent &agent);
	/*virtual*/ void 	idleUpdateTractorBeam();

	//--------------------------------------------------------------------
	// Loading state
	//--------------------------------------------------------------------
public:
	/*virtual*/ BOOL    getIsCloud();

	//--------------------------------------------------------------------
	// Region state
	//--------------------------------------------------------------------
	void			resetRegionCrossingTimer()	{ mRegionCrossingTimer.reset();	}

private:
	U64				mLastRegionHandle;
	LLFrameTimer	mRegionCrossingTimer;
	S32				mRegionCrossingCount;
	
/**                    State
 **                                                                            **
 *******************************************************************************/

/********************************************************************************
 **                                                                            **
 **                    RENDERING
 **/

	//--------------------------------------------------------------------
	// Render beam
	//--------------------------------------------------------------------
protected:
	BOOL 		needsRenderBeam();
private:
	LLPointer<LLHUDEffectSpiral> mBeam;
	LLFrameTimer mBeamTimer;

	//--------------------------------------------------------------------
	// LLVOAvatar Constants
	//--------------------------------------------------------------------
public:
	/*virtual*/ LLViewerTexture::EBoostLevel 	getAvatarBoostLevel() const { return LLViewerTexture::BOOST_AVATAR_SELF; }
	/*virtual*/ LLViewerTexture::EBoostLevel 	getAvatarBakedBoostLevel() const { return LLViewerTexture::BOOST_AVATAR_BAKED_SELF; }
	/*virtual*/ S32 						getTexImageSize() const { return LLVOAvatar::getTexImageSize()*4; }

/**                    Rendering
 **                                                                            **
 *******************************************************************************/

/********************************************************************************
 **                                                                            **
 **                    TEXTURES
 **/

	//--------------------------------------------------------------------
	// Loading status
	//--------------------------------------------------------------------
public:
	/*virtual*/ bool	hasPendingBakedUploads() const;
	S32					getLocalDiscardLevel(LLVOAvatarDefines::ETextureIndex type) const;
	bool				areTexturesCurrent() const;
	BOOL				isLocalTextureDataAvailable(const LLTexLayerSet* layerset) const;
	BOOL				isLocalTextureDataFinal(const LLTexLayerSet* layerset) const;
	/*virtual*/ BOOL    isTextureDefined(LLVOAvatarDefines::ETextureIndex type) const;
	/*virtual*/ BOOL	isTextureVisible(LLVOAvatarDefines::ETextureIndex type) const;
	//--------------------------------------------------------------------
	// Local Textures
	//--------------------------------------------------------------------
public:
	BOOL				getLocalTextureGL(LLVOAvatarDefines::ETextureIndex type, LLViewerTexture** image_gl_pp) const;
	LLViewerFetchedTexture*	getLocalTextureGL(LLVOAvatarDefines::ETextureIndex type) const;
	const LLUUID&		getLocalTextureID(LLVOAvatarDefines::ETextureIndex type) const;
	void				setLocalTextureTE( U8 te, LLViewerTexture* image, BOOL set_by_user );
	void 				setLocalTexture( LLVOAvatarDefines::ETextureIndex type, LLViewerTexture* tex, BOOL baked_version_exits );
protected:
	void				localTextureLoaded(BOOL succcess, LLViewerFetchedTexture *src_vi, LLImageRaw* src, LLImageRaw* aux_src, S32 discard_level, BOOL final, void* userdata);
	void				getLocalTextureByteCount(S32* gl_byte_count) const;
	/*virtual*/ void	addLocalTextureStats(LLVOAvatarDefines::ETextureIndex i, LLViewerFetchedTexture* imagep, F32 texel_area_ratio, BOOL rendered, BOOL covered_by_baked);
private:
	static void			onLocalTextureLoaded(BOOL succcess, LLViewerFetchedTexture *src_vi, LLImageRaw* src, LLImageRaw* aux_src, S32 discard_level, BOOL final, void* userdata);

	/*virtual*/	void	setImage(const U8 te, LLViewerTexture *imagep); 
	/*virtual*/ LLViewerTexture* getImage(const U8 te) const;


	//--------------------------------------------------------------------
	// Baked textures
	//--------------------------------------------------------------------
public:
	LLVOAvatarDefines::ETextureIndex getBakedTE(const LLTexLayerSet* layerset ) const;
	void				setNewBakedTexture(LLVOAvatarDefines::EBakedTextureIndex i, const LLUUID &uuid);
	void				setNewBakedTexture(LLVOAvatarDefines::ETextureIndex i, const LLUUID& uuid);
	void				setCachedBakedTexture(LLVOAvatarDefines::ETextureIndex i, const LLUUID& uuid);
	void				forceBakeAllTextures(bool slam_for_debug = false);
	static void			processRebakeAvatarTextures(LLMessageSystem* msg, void**);
	BOOL				isUsingBakedTextures() const; // e.g. false if in appearance edit mode
protected:
	/*virtual*/ void	removeMissingBakedTextures();

	//--------------------------------------------------------------------
	// Layers
	//--------------------------------------------------------------------
public:
	void 				requestLayerSetUploads();
	void				requestLayerSetUpload(LLVOAvatarDefines::EBakedTextureIndex i);
	void				requestLayerSetUpdate(LLVOAvatarDefines::ETextureIndex i);
	LLTexLayerSet*		getLayerSet(LLVOAvatarDefines::ETextureIndex index) const;
	
	//--------------------------------------------------------------------
	// Composites
	//--------------------------------------------------------------------
public:
	/* virtual */ void	invalidateComposite(LLTexLayerSet* layerset, BOOL upload_result);
	/* virtual */ void	invalidateAll();
	/* virtual */ void	setCompositeUpdatesEnabled(bool b); // only works for self
	/* virtual */ void  setCompositeUpdatesEnabled(U32 index, bool b);
	/* virtual */ bool 	isCompositeUpdateEnabled(U32 index);
	void				setupComposites();
	void				updateComposites();

	const LLUUID&		grabBakedTexture(LLVOAvatarDefines::EBakedTextureIndex baked_index) const;
	BOOL				canGrabBakedTexture(LLVOAvatarDefines::EBakedTextureIndex baked_index) const;


	//--------------------------------------------------------------------
	// Scratch textures (used for compositing)
	//--------------------------------------------------------------------
public:
	static void		deleteScratchTextures();
private:
	static S32 		sScratchTexBytes;
	static LLMap< LLGLenum, LLGLuint*> sScratchTexNames;
	static LLMap< LLGLenum, F32*> sScratchTexLastBindTime;

/**                    Textures
 **                                                                            **
 *******************************************************************************/

/********************************************************************************
 **                                                                            **
 **                    MESHES
 **/
protected:
	/*virtual*/ void   restoreMeshData();

/**                    Meshes
 **                                                                            **
 *******************************************************************************/

/********************************************************************************
 **                                                                            **
 **                    WEARABLES
 **/

public:
	/*virtual*/ BOOL	isWearingWearableType(LLWearableType::EType type) const;
	void				wearableUpdated(LLWearableType::EType type, BOOL upload_result);
protected:
	U32 getNumWearables(LLVOAvatarDefines::ETextureIndex i) const;

	//--------------------------------------------------------------------
	// Attachments
	//--------------------------------------------------------------------
public:
	void 				updateAttachmentVisibility(U32 camera_mode);
	BOOL 				isWearingAttachment(const LLUUID& inv_item_id) const;
	BOOL				attachmentWasRequested(const LLUUID& inv_item_id) const;
	void				addAttachmentRequest(const LLUUID& inv_item_id);
	void				removeAttachmentRequest(const LLUUID& inv_item_id);
	LLViewerObject* 	getWornAttachment(const LLUUID& inv_item_id);
	const std::string   getAttachedPointName(const LLUUID& inv_item_id) const;
	/*virtual*/ const LLViewerJointAttachment *attachObject(LLViewerObject *viewer_object);
	/*virtual*/ BOOL 	detachObject(LLViewerObject *viewer_object);
	static BOOL			detachAttachmentIntoInventory(const LLUUID& item_id);

private:
	// Track attachments that have been requested but have not arrived yet.
	mutable std::map<LLUUID,LLTimer> mAttachmentRequests;
	
	// <edit> testzone attachpt
public:
	void 			addUnsupportedAttachment( LLViewerObject *viewer_object );
	bool 			removeUnsupportedAttachment( LLViewerObject *viewer_object );
	BOOL 			isWearingUnsupportedAttachment( const LLUUID& inv_item_id );
	std::map<S32, std::pair<LLUUID/*inv*/,LLUUID/*object*/> > mUnsupportedAttachmentPoints;
	// </edit>
	
// [RLVa:KB] - Checked: 2010-03-14 (RLVa-1.2.0a) | Added: RLVa-1.1.0i
	LLViewerJointAttachment* getWornAttachmentPoint(const LLUUID& inv_item_id) const;
// [/RLVa:KB]

private:

	LLViewerJoint* 		mScreenp; // special purpose joint for HUD attachments
	
/**                    Attachments
 **                                                                            **
 *******************************************************************************/

/********************************************************************************
 **                                                                            **
 **                    APPEARANCE
 **/

public:
	static void		onCustomizeStart();
	static void		onCustomizeEnd();

	//--------------------------------------------------------------------
	// Visibility
	//--------------------------------------------------------------------
public:
	bool			sendAppearanceMessage(LLMessageSystem *mesgsys);// const;

/**                    Appearance
 **                                                                            **
 *******************************************************************************/

/********************************************************************************
 **                                                                            **
 **                    DIAGNOSTICS
 **/

	//--------------------------------------------------------------------
	// General
	//--------------------------------------------------------------------
public:	
	static void		dumpTotalLocalTextureByteCount();
	void			dumpLocalTextures() const;
	static void		dumpScratchTextureByteCount();
public:
	struct LLAvatarTexData
	{
		LLAvatarTexData(const LLUUID& id, LLVOAvatarDefines::ETextureIndex index) : 
			mAvatarID(id), 
			mIndex(index) 
		{}
		LLUUID			mAvatarID;
		LLVOAvatarDefines::ETextureIndex	mIndex;
	};
//Spechul schtuff.
	static void		onChangeSelfInvisible(BOOL newvalue);
	void			setInvisible(BOOL newvalue);
};

extern LLVOAvatarSelf *gAgentAvatarp;

BOOL isAgentAvatarValid();

#endif // LL_VO_AVATARSELF_H
