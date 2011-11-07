/** 
 * @file llvoavatar.cpp
 * @brief Implementation of LLVOAvatar class which is a derivation fo LLViewerObject
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
#include "llviewerprecompiledheaders.h"

#include "llvoavatarself.h"
#include "llvoavatar.h"

#include "pipeline.h"

#include "llagent.h" //  Get state values from here
#include "llagentcamera.h"
#include "llagentwearables.h"
#include "llhudeffecttrail.h"
#include "llhudmanager.h"
#include "llnotificationsutil.h"
#include "llselectmgr.h"
#include "lltexlayer.h"
#include "lltoolgrab.h"	// for needsRenderBeam
#include "lltoolmgr.h" // for needsRenderBeam
#include "lltoolmorph.h"
#include "llviewercamera.h"
#include "llviewercontrol.h"
#include "llviewermedia.h"
#include "llviewermenu.h"
#include "llviewerobjectlist.h"
#include "llviewerstats.h"
#include "llviewerregion.h"
#include "llmeshrepository.h"
#include "llvovolume.h"

#include "cofmgr.h"
#include "rlvhandler.h"

LLVOAvatarSelf *gAgentAvatarp = NULL;
BOOL isAgentAvatarValid()
{
	return (gAgentAvatarp &&
			(gAgentAvatarp->getRegion() != NULL) &&
			(!gAgentAvatarp->isDead()));
}

using namespace LLVOAvatarDefines;
//-----------------------------------------------------------------------------
// Static Data
//-----------------------------------------------------------------------------
S32 LLVOAvatarSelf::sScratchTexBytes = 0;
LLMap< LLGLenum, LLGLuint*> LLVOAvatarSelf::sScratchTexNames;
LLMap< LLGLenum, F32*> LLVOAvatarSelf::sScratchTexLastBindTime;


/*********************************************************************************
 **                                                                             **
 ** Begin LLVOAvatarSelf Constructor routines
 **
 **/

LLVOAvatarSelf::LLVOAvatarSelf(const LLUUID& id,
							   const LLPCode pcode,
							   LLViewerRegion* regionp) :
	LLVOAvatar(id, pcode, regionp),
	mScreenp(NULL),
	mLastRegionHandle(0),
	mRegionCrossingCount(0)
{
	gAgentWearables.setAvatarObject(this);
	gAgentCamera.setAvatarObject(this);

	mMotionController.mIsSelf = TRUE;

	for( S32 i=0; i<TEX_NUM_INDICES; i++ )
	{
		if (isIndexLocalTexture((ETextureIndex)i))
		{
			mLocalTextureData[(ETextureIndex)i] = LocalTextureData();
		}
	}

	lldebugs << "Marking avatar as self " << id << llendl;
}

void LLVOAvatarSelf::initInstance()
{
	BOOL status = TRUE;
	// creates hud joint(mScreen) among other things
	status &= loadAvatarSelf();

	// adds attachment points to mScreen among other things
	LLVOAvatar::initInstance();

	llinfos << "Self avatar object created. Starting timer." << llendl;
	/*mDebugSelfLoadTimer.reset();
	// clear all times to -1 for debugging
	for (U32 i =0; i < LLVOAvatarDefines::TEX_NUM_INDICES; ++i)
	{
		for (U32 j = 0; j <= MAX_DISCARD_LEVEL; ++j)
		{
			mDebugTextureLoadTimes[i][j] = -1.0f;
		}
	}

	for (U32 i =0; i < LLVOAvatarDefines::BAKED_NUM_INDICES; ++i)
	{
		mDebugBakedTextureTimes[i][0] = -1.0f;
		mDebugBakedTextureTimes[i][1] = -1.0f;
	}*/

	status &= buildMenus();
	if (!status)
	{
		llerrs << "Unable to load user's avatar" << llendl;
		return;
	}
}

// virtual
void LLVOAvatarSelf::markDead()
{
	mBeam = NULL;
	LLVOAvatar::markDead();
}

/*virtual*/ BOOL LLVOAvatarSelf::loadAvatar()
{
	BOOL success = LLVOAvatar::loadAvatar();
	return success;
}


BOOL LLVOAvatarSelf::loadAvatarSelf()
{
	BOOL success = TRUE;
	// avatar_skeleton.xml
	if (!buildSkeletonSelf(sAvatarSkeletonInfo))
	{
		llwarns << "avatar file: buildSkeleton() failed" << llendl;
		return FALSE;
	}
	// TODO: make loadLayersets() called only by self.
	//success &= loadLayersets();

	return success;
}

BOOL LLVOAvatarSelf::buildSkeletonSelf(const LLVOAvatarSkeletonInfo *info)
{
	LLMemType mt(LLMemType::MTYPE_AVATAR);

	// add special-purpose "screen" joint
	mScreenp = new LLViewerJoint("mScreen", NULL);
	// for now, put screen at origin, as it is only used during special
	// HUD rendering mode
	F32 aspect = LLViewerCamera::getInstance()->getAspect();
	LLVector3 scale(1.f, aspect, 1.f);
	mScreenp->setScale(scale);
	mScreenp->setWorldPosition(LLVector3::zero);
	// need to update screen agressively when sidebar opens/closes, for example
	mScreenp->mUpdateXform = TRUE;
	return TRUE;
}

BOOL LLVOAvatarSelf::buildMenus()
{
	//-------------------------------------------------------------------------
	// build the attach and detach menus
	//-------------------------------------------------------------------------
	if(gNoRender)
		return TRUE;
	// *TODO: Translate
	gAttachBodyPartPieMenus[0] = NULL;
	gAttachBodyPartPieMenus[1] = new LLPieMenu(std::string("Right Arm >"));
	gAttachBodyPartPieMenus[2] = new LLPieMenu(std::string("Head >"));
	gAttachBodyPartPieMenus[3] = new LLPieMenu(std::string("Left Arm >"));
	gAttachBodyPartPieMenus[4] = NULL;
	gAttachBodyPartPieMenus[5] = new LLPieMenu(std::string("Left Leg >"));
	gAttachBodyPartPieMenus[6] = new LLPieMenu(std::string("Torso >"));
	gAttachBodyPartPieMenus[7] = new LLPieMenu(std::string("Right Leg >"));

	gDetachBodyPartPieMenus[0] = NULL;
	gDetachBodyPartPieMenus[1] = new LLPieMenu(std::string("Right Arm >"));
	gDetachBodyPartPieMenus[2] = new LLPieMenu(std::string("Head >"));
	gDetachBodyPartPieMenus[3] = new LLPieMenu(std::string("Left Arm >"));
	gDetachBodyPartPieMenus[4] = NULL;
	gDetachBodyPartPieMenus[5] = new LLPieMenu(std::string("Left Leg >"));
	gDetachBodyPartPieMenus[6] = new LLPieMenu(std::string("Torso >"));
	gDetachBodyPartPieMenus[7] = new LLPieMenu(std::string("Right Leg >"));

	for (S32 i = 0; i < 8; i++)
	{
		if (gAttachBodyPartPieMenus[i])
		{
			gAttachPieMenu->appendPieMenu( gAttachBodyPartPieMenus[i] );
		}
		else
		{
			BOOL attachment_found = FALSE;
			for (attachment_map_t::iterator iter = mAttachmentPoints.begin(); 
				 iter != mAttachmentPoints.end();
				 ++iter)
			{
				LLViewerJointAttachment* attachment = iter->second;
				if (attachment->getGroup() == i)
				{
					LLMenuItemCallGL* item;
// [RLVa:KB] - Checked: 2009-07-06 (RLVa-1.0.0c)
					// We need the userdata param to disable options in this pie menu later on (Left Hand / Right Hand option)
					item = new LLMenuItemCallGL(attachment->getName(), 
												NULL, 
												object_selected_and_point_valid, attachment);
// [/RLVa:KB]
//						item = new LLMenuItemCallGL(attachment->getName(), 
//													NULL, 
//													object_selected_and_point_valid);
					item->addListener(gMenuHolder->getListenerByName("Object.AttachToAvatar"), "on_click", iter->first);
					
					gAttachPieMenu->append(item);

					attachment_found = TRUE;
					break;

				}
			}







			if (!attachment_found)
			{
				gAttachPieMenu->appendSeparator();
			}
		}

		if (gDetachBodyPartPieMenus[i])
		{
			gDetachPieMenu->appendPieMenu( gDetachBodyPartPieMenus[i] );
		}
		else
		{
			BOOL attachment_found = FALSE;
			for (attachment_map_t::iterator iter = mAttachmentPoints.begin(); 
				 iter != mAttachmentPoints.end();
				 ++iter)
			{
				LLViewerJointAttachment* attachment = iter->second;
				if (attachment->getGroup() == i)
				{
					gDetachPieMenu->append(new LLMenuItemCallGL(attachment->getName(),
						&handle_detach_from_avatar, object_attached, attachment));

					attachment_found = TRUE;
					break;
				}
			}

			if (!attachment_found)
			{
				gDetachPieMenu->appendSeparator();
			}
		}
	}

	// add screen attachments
	for (attachment_map_t::iterator iter = mAttachmentPoints.begin();
		 iter != mAttachmentPoints.end();
		 ++iter)
	{
		LLViewerJointAttachment* attachment = iter->second;
		if (attachment->getGroup() == 8)
		{
			LLMenuItemCallGL* item;
// [RLVa:KB] - Checked: 2009-07-06 (RLVa-1.0.0c)
			// We need the userdata param to disable options in this pie menu later on
			item = new LLMenuItemCallGL(attachment->getName(), 
										NULL, 
										object_selected_and_point_valid, attachment);
// [/RLVa:KB]
//				item = new LLMenuItemCallGL(attachment->getName(), 
//											NULL, 
//											object_selected_and_point_valid);
			item->addListener(gMenuHolder->getListenerByName("Object.AttachToAvatar"), "on_click", iter->first);
			gAttachScreenPieMenu->append(item);
			gDetachScreenPieMenu->append(new LLMenuItemCallGL(attachment->getName(), 
						&handle_detach_from_avatar, object_attached, attachment));
		}
	}



	for (S32 pass = 0; pass < 2; pass++)
	{
		// *TODO: Skinning - gAttachSubMenu is an awful, awful hack
		if (!gAttachSubMenu)
		{
			break;
		}
		for (attachment_map_t::iterator iter = mAttachmentPoints.begin(); 
			 iter != mAttachmentPoints.end();
			 ++iter)
		{
			LLViewerJointAttachment* attachment = iter->second;
			if (attachment->getIsHUDAttachment() != (pass == 1))
			{
				continue;
			}
			// RELEASE-RLVa: random comment because we want know if LL ever changes this to not include "attachment" as userdata
			LLMenuItemCallGL* item = new LLMenuItemCallGL(attachment->getName(), 
														  NULL, &object_selected_and_point_valid,
														  &attach_label, attachment);
			item->addListener(gMenuHolder->getListenerByName("Object.AttachToAvatar"), "on_click", iter->first);
			gAttachSubMenu->append(item);

			gDetachSubMenu->append(new LLMenuItemCallGL(attachment->getName(), 
				&handle_detach_from_avatar, object_attached, &detach_label, attachment));
			
		}
		if (pass == 0)
		{
			// put separator between non-hud and hud attachments
			gAttachSubMenu->appendSeparator();
			gDetachSubMenu->appendSeparator();
		}
	}

	for (S32 group = 0; group < 8; group++)
	{
		// skip over groups that don't have sub menus
		if (!gAttachBodyPartPieMenus[group] || !gDetachBodyPartPieMenus[group])
		{
			continue;
		}

		std::multimap<S32, S32> attachment_pie_menu_map;

		// gather up all attachment points assigned to this group, and throw into map sorted by pie slice number
		for (attachment_map_t::iterator iter = mAttachmentPoints.begin(); 
			 iter != mAttachmentPoints.end();
			 ++iter)
		{
			LLViewerJointAttachment* attachment = iter->second;
			if(attachment->getGroup() == group)
			{
				// use multimap to provide a partial order off of the pie slice key
				S32 pie_index = attachment->getPieSlice();
				attachment_pie_menu_map.insert(std::make_pair(pie_index, iter->first));
			}
		}

		// add in requested order to pie menu, inserting separators as necessary
		S32 cur_pie_slice = 0;
		for (std::multimap<S32, S32>::iterator attach_it = attachment_pie_menu_map.begin();
			 attach_it != attachment_pie_menu_map.end(); ++attach_it)
		{
			S32 requested_pie_slice = attach_it->first;
			S32 attach_index = attach_it->second;
			while (cur_pie_slice < requested_pie_slice)
			{
				gAttachBodyPartPieMenus[group]->appendSeparator();
				gDetachBodyPartPieMenus[group]->appendSeparator();
				cur_pie_slice++;
			}

			LLViewerJointAttachment* attachment = get_if_there(mAttachmentPoints, attach_index, (LLViewerJointAttachment*)NULL);
			if (attachment)
			{
// [RLVa:KB] - Checked: 2009-07-06 (RLVa-1.0.0c)
				// We need the userdata param to disable options in this pie menu later on
				LLMenuItemCallGL* item = new LLMenuItemCallGL(attachment->getName(), 
															  NULL, object_selected_and_point_valid, attachment);
// [/RLVa:KB]
//					LLMenuItemCallGL* item = new LLMenuItemCallGL(attachment->getName(), 
//																  NULL, object_selected_and_point_valid);
				gAttachBodyPartPieMenus[group]->append(item);
				item->addListener(gMenuHolder->getListenerByName("Object.AttachToAvatar"), "on_click", attach_index);
				gDetachBodyPartPieMenus[group]->append(new LLMenuItemCallGL(attachment->getName(), 
																			&handle_detach_from_avatar,
																			object_attached, attachment));
				cur_pie_slice++;
			}
		}
	}
	return TRUE;
}

void LLVOAvatarSelf::cleanup()
{
	markDead();
 	delete mScreenp;
 	mScreenp = NULL;
	mRegionp = NULL;
}

LLVOAvatarSelf::~LLVOAvatarSelf()
{
	cleanup();
}

/**
 **
 ** End LLVOAvatarSelf Constructor routines
 **                                                                             **
 *********************************************************************************/

//virtual
BOOL LLVOAvatarSelf::loadLayersets()
{
	BOOL success = TRUE;
	for (LLVOAvatarXmlInfo::layer_info_list_t::const_iterator iter = sAvatarXmlInfo->mLayerInfoList.begin();
		 iter != sAvatarXmlInfo->mLayerInfoList.end(); 
		 ++iter)
	{
		// Construct a layerset for each one specified in avatar_lad.xml and initialize it as such.
		const LLTexLayerSetInfo *info = *iter;
		LLTexLayerSet* layer_set = new LLTexLayerSet( this );
		
		if (!layer_set->setInfo(info))
		{
			stop_glerror();
			delete layer_set;
			llwarns << "avatar file: layer_set->parseData() failed" << llendl;
			return FALSE;
		}

		// scan baked textures and associate the layerset with the appropriate one
		EBakedTextureIndex baked_index = BAKED_NUM_INDICES;
		for (LLVOAvatarDictionary::BakedTextures::const_iterator baked_iter = LLVOAvatarDictionary::getInstance()->getBakedTextures().begin();
			 baked_iter != LLVOAvatarDictionary::getInstance()->getBakedTextures().end();
			 ++baked_iter)
		{
			const LLVOAvatarDictionary::BakedEntry *baked_dict = baked_iter->second;
			if (layer_set->isBodyRegion(baked_dict->mName))
			{
				baked_index = baked_iter->first;
				// ensure both structures are aware of each other
				mBakedTextureDatas[baked_index].mTexLayerSet = layer_set;
				layer_set->setBakedTexIndex(baked_index);
				break;
			}
		}
		// if no baked texture was found, warn and cleanup
		if (baked_index == BAKED_NUM_INDICES)
		{
			llwarns << "<layer_set> has invalid body_region attribute" << llendl;
			delete layer_set;
			return FALSE;
		}

		// scan morph masks and let any affected layers know they have an associated morph
		for (LLVOAvatar::morph_list_t::const_iterator morph_iter = mBakedTextureDatas[baked_index].mMaskedMorphs.begin();
			morph_iter != mBakedTextureDatas[baked_index].mMaskedMorphs.end();
			 ++morph_iter)
		{
			LLMaskedMorph *morph = *morph_iter;
			LLTexLayerInterface* layer = layer_set->findLayerByName(morph->mLayer);
			if (layer)
			{
				layer->setHasMorph(TRUE);
			}
			else
			{
				llwarns << "Could not find layer named " << morph->mLayer << " to set morph flag" << llendl;
				success = FALSE;
			}
		}
	}
	return success;
}
// virtual
BOOL LLVOAvatarSelf::updateCharacter(LLAgent &agent)
{
	LLMemType mt(LLMemType::MTYPE_AVATAR);

	// update screen joint size
	if (mScreenp)
	{
		F32 aspect = LLViewerCamera::getInstance()->getAspect();
		LLVector3 scale(1.f, aspect, 1.f);
		mScreenp->setScale(scale);
		mScreenp->updateWorldMatrixChildren();
		resetHUDAttachments();
	}
	
	return LLVOAvatar::updateCharacter(agent);
}

// virtual
BOOL LLVOAvatarSelf::idleUpdate(LLAgent &agent, LLWorld &world, const F64 &time)
{
	if (!isAgentAvatarValid())
	{
		return TRUE;
	}
	if(!gNoRender)
	{
		//Emerald performs some force-bakes stuff here. Added it in because we noticed slow responses with client tag ident. -HgB
		for(U8 i=0;i<getNumTEs();++i)
		{
			LLViewerTexture* te = getTEImage(i);
			te->forceActive();
		}
	}
	LLVOAvatar::idleUpdate(agent,world,time);
	if(!gNoRender)
		idleUpdateTractorBeam();
	return TRUE;	
}

// virtual
LLJoint *LLVOAvatarSelf::getJoint(const std::string &name)
{
	if (mScreenp)
	{
		LLJoint* jointp = mScreenp->findJoint(name);
		if (jointp) return jointp;
	}
	return LLVOAvatar::getJoint(name);
}
//virtual
void LLVOAvatarSelf::resetJointPositions( void )
{
	return LLVOAvatar::resetJointPositions();
}
// virtual
BOOL LLVOAvatarSelf::setVisualParamWeight(LLVisualParam *which_param, F32 weight, BOOL upload_bake )
{
	if (!which_param)
	{
		return FALSE;
	}
	LLViewerVisualParam *param = (LLViewerVisualParam*) LLCharacter::getVisualParam(which_param->getID());
	return setParamWeight(param,weight,upload_bake);
}

// virtual
BOOL LLVOAvatarSelf::setVisualParamWeight(const char* param_name, F32 weight, BOOL upload_bake )
{
	if (!param_name)
	{
		return FALSE;
	}
	LLViewerVisualParam *param = (LLViewerVisualParam*) LLCharacter::getVisualParam(param_name);
	return setParamWeight(param,weight,upload_bake);
}

// virtual
BOOL LLVOAvatarSelf::setVisualParamWeight(S32 index, F32 weight, BOOL upload_bake )
{
	LLViewerVisualParam *param = (LLViewerVisualParam*) LLCharacter::getVisualParam(index);
	return setParamWeight(param,weight,upload_bake);
}

BOOL LLVOAvatarSelf::setParamWeight(LLViewerVisualParam *param, F32 weight, BOOL upload_bake )
{
	if (!param)
	{
		return FALSE;
	}

	/*if (param->getCrossWearable())
	{
		LLWearableType::EType type = (LLWearableType::EType)param->getWearableType();
		U32 size = gAgentWearables.getWearableCount(type);
		for (U32 count = 0; count < size; ++count)
		{
			LLWearable *wearable = gAgentWearables.getWearable(type,count);
			if (wearable)
			{
				wearable->setVisualParamWeight(param->getID(), weight, upload_bake);
			}
		}
	}*/

	return LLCharacter::setVisualParamWeight(param,weight,upload_bake);
}

/*virtual*/ 
void LLVOAvatarSelf::updateVisualParams()
{
	LLVOAvatar::updateVisualParams();
}

/*virtual*/
void LLVOAvatarSelf::idleUpdateAppearanceAnimation()
{
	// Animate all top-level wearable visual parameters
	/*gAgentWearables.animateAllWearableParams(calcMorphAmount(), FALSE);

	// apply wearable visual params to avatar
	for (U32 type = 0; type < LLWearableType::WT_COUNT; type++)
	{
		LLWearable *wearable = gAgentWearables.getTopWearable((LLWearableType::EType)type);
		if (wearable)
		{
			wearable->writeToAvatar();
		}
	}*/

	//allow avatar to process updates
	LLVOAvatar::idleUpdateAppearanceAnimation();

}

// virtual
void LLVOAvatarSelf::requestStopMotion(LLMotion* motion)
{
	// Only agent avatars should handle the stop motion notifications.

	// Notify agent that motion has stopped
	gAgent.requestStopMotion(motion);
}

// virtual
void LLVOAvatarSelf::stopMotionFromSource(const LLUUID& source_id)
{
	for(AnimSourceIterator motion_it = mAnimationSources.find(source_id); motion_it != mAnimationSources.end();)
	{
		gAgent.sendAnimationRequest( motion_it->second, ANIM_REQUEST_STOP );
		mAnimationSources.erase(motion_it++);
	}

	LLViewerObject* object = gObjectList.findObject(source_id);
	if (object)
	{
		object->mFlags &= ~FLAGS_ANIM_SOURCE;
	}
}

//-----------------------------------------------------------------------------
// setLocalTextureTE()
//-----------------------------------------------------------------------------
void LLVOAvatarSelf::setLocalTextureTE( U8 te, LLViewerTexture* image, BOOL set_by_user )
{
	if( te >= TEX_NUM_INDICES )
	{
		llassert(0);
		return;
	}

	if( getTEImage( te )->getID() == image->getID() )
	{
		return;
	}

	if (isIndexBakedTexture((ETextureIndex)te))
	{
		llassert(0);
		return;
	}

	LLTexLayerSet* layer_set = getLayerSet((ETextureIndex)te);
	if (layer_set)
	{
		invalidateComposite(layer_set, set_by_user);
	}

	setTEImage( te, image );
	updateMeshTextures();

	if( gAgentCamera.cameraCustomizeAvatar() )
	{
		LLVisualParamHint::requestHintUpdates();
	}
}
//virtual
void LLVOAvatarSelf::removeMissingBakedTextures()
{
	BOOL removed = FALSE;
	for (U32 i = 0; i < mBakedTextureDatas.size(); i++)
	{
		const S32 te = mBakedTextureDatas[i].mTextureIndex;
		const LLViewerTexture* tex = getTEImage(te);

		// Replace with default if we can't find the asset, assuming the
		// default is actually valid (which it should be unless something
		// is seriously wrong).
		if (!tex || tex->isMissingAsset())
		{
			LLViewerTexture *imagep = LLViewerTextureManager::getFetchedTexture(IMG_DEFAULT_AVATAR);
			if (imagep)
			{
				setTEImage(te, imagep);
				removed = TRUE;
			}
		}
	}

	if (removed)
	{
		for(U32 i = 0; i < mBakedTextureDatas.size(); i++)
		{
			mBakedTextureDatas[i].mTexLayerSet->setUpdatesEnabled(TRUE);
			invalidateComposite(mBakedTextureDatas[i].mTexLayerSet, FALSE);
		}
		updateMeshTextures();
		requestLayerSetUploads();
	}
}

//virtual
void LLVOAvatarSelf::updateRegion(LLViewerRegion *regionp)
{
	// Save the global position
	LLVector3d global_pos_from_old_region = getPositionGlobal();

	// Change the region
	setRegion(regionp);

	if (regionp)
	{	// Set correct region-relative position from global coordinates
		setPositionGlobal(global_pos_from_old_region);

		// Diagnostic info
		//LLVector3d pos_from_new_region = getPositionGlobal();
		//llinfos << "pos_from_old_region is " << global_pos_from_old_region
		//	<< " while pos_from_new_region is " << pos_from_new_region
		//	<< llendl;
	}

	if (!regionp || (regionp->getHandle() != mLastRegionHandle))
	{
		if (mLastRegionHandle != 0)
		{
			++mRegionCrossingCount;
			F64 delta = (F64)mRegionCrossingTimer.getElapsedTimeF32();
			F64 avg = (mRegionCrossingCount == 1) ? 0 : LLViewerStats::getInstance()->getStat(LLViewerStats::ST_CROSSING_AVG);
			F64 delta_avg = (delta + avg*(mRegionCrossingCount-1)) / mRegionCrossingCount;
			LLViewerStats::getInstance()->setStat(LLViewerStats::ST_CROSSING_AVG, delta_avg);

			F64 max = (mRegionCrossingCount == 1) ? 0 : LLViewerStats::getInstance()->getStat(LLViewerStats::ST_CROSSING_MAX);
			max = llmax(delta, max);
			LLViewerStats::getInstance()->setStat(LLViewerStats::ST_CROSSING_MAX, max);

			// Diagnostics
			llinfos << "Region crossing took " << (F32)(delta * 1000.0) << " ms " << llendl;
		}
		if (regionp)
		{
			mLastRegionHandle = regionp->getHandle();
		}
	}
	mRegionCrossingTimer.reset();
	LLViewerObject::updateRegion(regionp);
}

//--------------------------------------------------------------------
// draw tractor beam when editing objects
//--------------------------------------------------------------------
//virtual
void LLVOAvatarSelf::idleUpdateTractorBeam()
{


	if(gSavedSettings.getBOOL("DisablePointAtAndBeam"))
	{
		return;
	}
	// </edit>

	const LLPickInfo& pick = gViewerWindow->getLastPick();

	// No beam for media textures
	// TODO: this will change for Media on a Prim
	if(pick.getObject() && pick.mObjectFace >= 0)
	{
		const LLTextureEntry* tep = pick.getObject()->getTE(pick.mObjectFace);
		if (tep && LLViewerMedia::textureHasMedia(tep->getID()))
		{
			return;
		}
	}

	// This is only done for yourself (maybe it should be in the agent?)
	if (!needsRenderBeam() || !mIsBuilt)
	{
		mBeam = NULL;
	}
	else if (!mBeam || mBeam->isDead())
	{
		// VEFFECT: Tractor Beam
		mBeam = (LLHUDEffectSpiral *)LLHUDManager::getInstance()->createViewerEffect(LLHUDObject::LL_HUD_EFFECT_BEAM);
		mBeam->setColor(LLColor4U(gAgent.getEffectColor()));
		mBeam->setSourceObject(this);
		mBeamTimer.reset();
	}

	if (!mBeam.isNull())
	{
		LLObjectSelectionHandle selection = LLSelectMgr::getInstance()->getSelection();

		if (gAgentCamera.mPointAt.notNull())
		{
			// get point from pointat effect
			mBeam->setPositionGlobal(gAgentCamera.mPointAt->getPointAtPosGlobal());
			mBeam->triggerLocal();
		}
		else if (selection->getFirstRootObject() && 
				selection->getSelectType() != SELECT_TYPE_HUD)
		{
			LLViewerObject* objectp = selection->getFirstRootObject();
			mBeam->setTargetObject(objectp);
		}
		else
		{
			mBeam->setTargetObject(NULL);
			LLTool *tool = LLToolMgr::getInstance()->getCurrentTool();
			if (tool->isEditing())
			{
				if (tool->getEditingObject())
				{
					mBeam->setTargetObject(tool->getEditingObject());
				}
				else
				{
					mBeam->setPositionGlobal(tool->getEditingPointGlobal());
				}
			}
			else
			{
				
				mBeam->setPositionGlobal(pick.mPosGlobal);
			}

		}
		if (mBeamTimer.getElapsedTimeF32() > 0.25f)
		{
			mBeam->setColor(LLColor4U(gAgent.getEffectColor()));
			mBeam->setNeedsSendToSim(TRUE);
			mBeamTimer.reset();
		}
	}
}

//-----------------------------------------------------------------------------
// restoreMeshData()
//-----------------------------------------------------------------------------
// virtual
void LLVOAvatarSelf::restoreMeshData()
{
	LLMemType mt(LLMemType::MTYPE_AVATAR);
	
	//llinfos << "Restoring" << llendl;
	mMeshValid = TRUE;
	updateJointLODs();
	updateAttachmentVisibility(gAgentCamera.getCameraMode());

	// force mesh update as LOD might not have changed to trigger this
	gPipeline.markRebuild(mDrawable, LLDrawable::REBUILD_GEOMETRY, TRUE);
}



//-----------------------------------------------------------------------------
// updateAttachmentVisibility()
//-----------------------------------------------------------------------------
void LLVOAvatarSelf::updateAttachmentVisibility(U32 camera_mode)
{
	for (attachment_map_t::iterator iter = mAttachmentPoints.begin(); 
		 iter != mAttachmentPoints.end();
		 ++iter)
	{
		LLViewerJointAttachment* attachment = iter->second;
		if (attachment->getIsHUDAttachment())
		{
			attachment->setAttachmentVisibility(TRUE);
		}
		else
		{
			switch (camera_mode)
			{
			case CAMERA_MODE_MOUSELOOK:
				if (LLVOAvatar::sVisibleInFirstPerson && attachment->getVisibleInFirstPerson())
				{
					attachment->setAttachmentVisibility(TRUE);
				}
				else
				{
					attachment->setAttachmentVisibility(FALSE);
				}
				break;
			default:
				attachment->setAttachmentVisibility(TRUE);
				break;
			}
		}
	}
}

/*virtual*/ BOOL LLVOAvatarSelf::isWearingWearableType(LLWearableType::EType type ) const
{
	return gAgentWearables.getWearableCount(type) > 0;
}

//-----------------------------------------------------------------------------
// wearableUpdated(EWearableType type, BOOL upload_result)
// forces an update to any baked textures relevant to type.
// will force an upload of the resulting bake if the second parameter is TRUE
//-----------------------------------------------------------------------------
void LLVOAvatarSelf::wearableUpdated(LLWearableType::EType type, BOOL upload_result)
{
	for (LLVOAvatarDictionary::BakedTextures::const_iterator baked_iter = LLVOAvatarDictionary::getInstance()->getBakedTextures().begin();
		baked_iter != LLVOAvatarDictionary::getInstance()->getBakedTextures().end();
	 	++baked_iter)
	{
		const LLVOAvatarDictionary::BakedEntry *baked_dict = baked_iter->second;
		const LLVOAvatarDefines::EBakedTextureIndex index = baked_iter->first;

		if (baked_dict)
		{
			for (LLVOAvatarDefines::wearables_vec_t::const_iterator type_iter = baked_dict->mWearables.begin();
				type_iter != baked_dict->mWearables.end();
				 ++type_iter)
			{
				const LLWearableType::EType comp_type = *type_iter;
				if (comp_type == type)
				{
					if (mBakedTextureDatas[index].mTexLayerSet)
					{
						mBakedTextureDatas[index].mTexLayerSet->setUpdatesEnabled(true);
						invalidateComposite(mBakedTextureDatas[index].mTexLayerSet, upload_result);
					}
					break;
				}
			}
		}
	}
	
	// Physics type has no associated baked textures, but change of params needs to be sent to
	// other avatars.
	if (type == LLWearableType::WT_PHYSICS)
	  {
	    gAgent.sendAgentSetAppearance();
	  }
}

//-----------------------------------------------------------------------------
// isWearingAttachment()
//-----------------------------------------------------------------------------
BOOL LLVOAvatarSelf::isWearingAttachment( const LLUUID& inv_item_id ) const
{
	const LLUUID& base_inv_item_id = gInventory.getLinkedItemID(inv_item_id);
	for (attachment_map_t::const_iterator iter = mAttachmentPoints.begin(); 
		 iter != mAttachmentPoints.end();
		 ++iter)
	{
		const LLViewerJointAttachment* attachment = iter->second;
		if(attachment->getAttachedObject(base_inv_item_id))
		{
			return TRUE;
		}
	}
	return FALSE;
}

//-----------------------------------------------------------------------------
BOOL LLVOAvatarSelf::attachmentWasRequested(const LLUUID& inv_item_id) const
{
	const F32 REQUEST_EXPIRATION_SECONDS = 5.0;  // any request older than this is ignored/removed.
	std::map<LLUUID,LLTimer>::iterator it = mAttachmentRequests.find(inv_item_id);
	if (it != mAttachmentRequests.end())
	{
		const LLTimer& request_time = it->second;
		F32 request_time_elapsed = request_time.getElapsedTimeF32();
		if (request_time_elapsed > REQUEST_EXPIRATION_SECONDS)
		{
			mAttachmentRequests.erase(it);
			return FALSE;
		}
		else
		{
			return TRUE;
		}
	}
	else
	{
		return FALSE;
	}
}

//-----------------------------------------------------------------------------
void LLVOAvatarSelf::addAttachmentRequest(const LLUUID& inv_item_id)
{
	LLTimer current_time;
	mAttachmentRequests[inv_item_id] = current_time;
}

//-----------------------------------------------------------------------------
void LLVOAvatarSelf::removeAttachmentRequest(const LLUUID& inv_item_id)
{
	mAttachmentRequests.erase(inv_item_id);
}

//-----------------------------------------------------------------------------
// getWornAttachment()
//-----------------------------------------------------------------------------
LLViewerObject* LLVOAvatarSelf::getWornAttachment(const LLUUID& inv_item_id)
{
	const LLUUID& base_inv_item_id = gInventory.getLinkedItemID(inv_item_id);
	for (attachment_map_t::const_iterator iter = mAttachmentPoints.begin(); 
		 iter != mAttachmentPoints.end();
		 ++iter)
	{
		LLViewerJointAttachment* attachment = iter->second;
 		if (LLViewerObject *attached_object = attachment->getAttachedObject(base_inv_item_id))
		{
			return attached_object;
		}
	}
	return NULL;
}

// [RLVa:KB] - Checked: 2010-03-14 (RLVa-1.2.0a) | Modified: RLVa-1.2.0a
LLViewerJointAttachment* LLVOAvatarSelf::getWornAttachmentPoint(const LLUUID& idItem) const
{
	const LLUUID& idItemBase = gInventory.getLinkedItemID(idItem);
	for (attachment_map_t::const_iterator itAttachPt = mAttachmentPoints.begin(); itAttachPt != mAttachmentPoints.end(); ++itAttachPt)
	{
		LLViewerJointAttachment* pAttachPt = itAttachPt->second;
 		if (pAttachPt->getAttachedObject(idItemBase))
			return pAttachPt;
	}
	return NULL;
}
// [/RLVa:KB]

const std::string LLVOAvatarSelf::getAttachedPointName(const LLUUID& inv_item_id) const
{
	const LLUUID& base_inv_item_id = gInventory.getLinkedItemID(inv_item_id);
	for (attachment_map_t::const_iterator iter = mAttachmentPoints.begin();
		 iter != mAttachmentPoints.end(); 
		 ++iter)
	{
		const LLViewerJointAttachment* attachment = iter->second;
		if (attachment->getAttachedObject(base_inv_item_id))
		{
			return attachment->getName();
		}
	}

	return LLStringUtil::null;
}

//virtual
const LLViewerJointAttachment *LLVOAvatarSelf::attachObject(LLViewerObject *viewer_object)
{
	const LLViewerJointAttachment *attachment = LLVOAvatar::attachObject(viewer_object);
	if(!attachment)
	{
// <edit>
		addUnsupportedAttachment(viewer_object);
// </edit>
		return 0;
	}
	
	updateAttachmentVisibility(gAgentCamera.getCameraMode());
		
// [RLVa:KB] - Checked: 2010-08-22 (RLVa-1.2.1a) | Modified: RLVa-1.2.1a
	// NOTE: RLVa event handlers should be invoked *after* LLVOAvatar::attachObject() calls LLViewerJointAttachment::addObject()
	if (rlv_handler_t::isEnabled())
	{
		RlvAttachmentLockWatchdog::instance().onAttach(viewer_object, attachment);
		gRlvHandler.onAttach(viewer_object, attachment);

		if ( (attachment->getIsHUDAttachment()) && (!gRlvAttachmentLocks.hasLockedHUD()) )
			gRlvAttachmentLocks.updateLockedHUD();
	}
// [/RLVa:KB]

	// Then make sure the inventory is in sync with the avatar.
	gInventory.addChangedMask(LLInventoryObserver::LABEL, viewer_object->getAttachmentItemID());
	gInventory.notifyObservers();

	// Should just be the last object added
	if (attachment->isObjectAttached(viewer_object))
	{
		const LLUUID& attachment_id = viewer_object->getAttachmentItemID();
		LLCOFMgr::instance().addAttachment(attachment_id);
		// Clear any pending requests once the attachment arrives.
		removeAttachmentRequest(attachment_id);
		updateLODRiggedAttachments();
	}

	return attachment;
}

//virtual
BOOL LLVOAvatarSelf::detachObject(LLViewerObject *viewer_object)
{
	const LLUUID attachment_id = viewer_object->getAttachmentItemID();
	const std::string point_name = getAttachedPointName(attachment_id);
	if(LLVOAvatar::detachObject(viewer_object))
	{
		LLVOAvatar::cleanupAttachedMesh( viewer_object );
		
		// the simulator should automatically handle permission revocation
		
		stopMotionFromSource(attachment_id);
		LLFollowCamMgr::setCameraActive(viewer_object->getID(), FALSE);

		LLViewerObject::const_child_list_t& child_list = viewer_object->getChildren();
		for (LLViewerObject::child_list_t::const_iterator iter = child_list.begin();
			 iter != child_list.end(); 
			 ++iter)
		{
			LLViewerObject* child_objectp = *iter;
					// the simulator should automatically handle
					// permissions revocation

			stopMotionFromSource(child_objectp->getID());
			LLFollowCamMgr::setCameraActive(child_objectp->getID(), FALSE);
		}

// [RLVa:KB] - Checked: 2010-08-22 (RLVa-1.2.1a) | Added: RLVa-1.2.1a
		if ( (rlv_handler_t::isEnabled()) && (viewer_object->isHUDAttachment()) && (gRlvAttachmentLocks.hasLockedHUD()) )
			gRlvAttachmentLocks.updateLockedHUD();
// [/RLVa:KB]
	
		lldebugs << "Detaching object " << viewer_object->mID << " from " << point_name << llendl;

		// Then make sure the inventory is in sync with the avatar.
		gInventory.addChangedMask(LLInventoryObserver::LABEL, attachment_id);
		gInventory.notifyObservers();

		// Update COF contents (unless the avatar is being destroyed)
		if (!isAgentAvatarValid())
		{
			llinfos << "removeItemLinks skipped, avatar is under destruction" << llendl;
		}
		else
		{
			LLCOFMgr::instance().removeAttachment(attachment_id);
		}
		return TRUE;
	}
	
// <edit>
	if(removeUnsupportedAttachment(viewer_object))
		return TRUE;
// </edit>
	

	return FALSE;
}

// static
BOOL LLVOAvatarSelf::detachAttachmentIntoInventory(const LLUUID &item_id)
{
	LLInventoryItem* item = gInventory.getLinkedItem(item_id);
	if ( (item) && (gAgentAvatarp) && (!gAgentAvatarp->isWearingAttachment(item->getUUID())) )
	{
		LLCOFMgr::instance().removeAttachment(item->getUUID());
		return FALSE;
	}
//	if (item)
// [RLVa:KB] - Checked: 2010-09-04 (RLVa-1.2.1c) | Added: RLVa-1.2.1c
	if ( (item) && ((!rlv_handler_t::isEnabled()) || (gRlvAttachmentLocks.canDetach(item))) )
// [/RLVa:KB]
	{
		gMessageSystem->newMessageFast(_PREHASH_DetachAttachmentIntoInv);
		gMessageSystem->nextBlockFast(_PREHASH_ObjectData);
		gMessageSystem->addUUIDFast(_PREHASH_AgentID, gAgent.getID());
		gMessageSystem->addUUIDFast(_PREHASH_ItemID, item_id);
		gMessageSystem->sendReliable(gAgent.getRegion()->getHost());
		
		// This object might have been selected, so let the selection manager know it's gone now
		LLViewerObject *found_obj = gObjectList.findObject(item_id);
		if (found_obj)
		{
			LLSelectMgr::getInstance()->remove(found_obj);
		}

		return TRUE;
	}
	return FALSE;
}

// <edit> testzone attachpt
void LLVOAvatarSelf::addUnsupportedAttachment(LLViewerObject *viewer_object)
{

	S32 attachmentID = ATTACHMENT_ID_FROM_STATE(viewer_object->getState());
	LLUUID item_id;
	LLNameValue* item_id_nv = viewer_object->getNVPair("AttachItemID");
	if( item_id_nv )
	{
		const char* s = item_id_nv->getString();
		if(s)
			item_id.set(s);
	}
	if(!item_id.isNull())
	{
		mUnsupportedAttachmentPoints[attachmentID] = std::pair<LLUUID,LLUUID>(item_id,viewer_object->getID());
		if (viewer_object->isSelected())
		{
			LLSelectMgr::getInstance()->updateSelectionCenter();
			LLSelectMgr::getInstance()->updatePointAt();
		}

		updateAttachmentVisibility(gAgentCamera.getCameraMode());
				
			// Then make sure the inventory is in sync with the avatar.
		gInventory.addChangedMask( LLInventoryObserver::LABEL, item_id );
		gInventory.notifyObservers();
	}
	else
		llwarns << "No item ID" << llendl;

}
bool LLVOAvatarSelf::removeUnsupportedAttachment(LLViewerObject *viewer_object)
{
	LLUUID item_id;
	LLNameValue* item_id_nv = viewer_object->getNVPair("AttachItemID");
	if( item_id_nv )
	{
		const char* s = item_id_nv->getString();
		if(s)
			item_id.set(s);
	}
	if(!item_id.isNull())
	{
		std::map<S32, std::pair<LLUUID,LLUUID> >::iterator iter = mUnsupportedAttachmentPoints.begin();
		std::map<S32, std::pair<LLUUID,LLUUID> >::iterator end = mUnsupportedAttachmentPoints.end();
		for( ; iter != end; ++iter)
		{
			if((*iter).second.first == item_id)
			{
				mUnsupportedAttachmentPoints.erase((*iter).first);
				// the simulator should automatically handle
				// permission revocation

				stopMotionFromSource(viewer_object->getID());
				LLFollowCamMgr::setCameraActive(viewer_object->getID(), FALSE);

				LLViewerObject::const_child_list_t& child_list = viewer_object->getChildren();
				for (LLViewerObject::child_list_t::const_iterator iter = child_list.begin();
					 iter != child_list.end(); iter++)
				{
					LLViewerObject* child_objectp = *iter;
					// the simulator should automatically handle
					// permissions revocation

					stopMotionFromSource(child_objectp->getID());
					LLFollowCamMgr::setCameraActive(child_objectp->getID(), FALSE);
				}
				// Then make sure the inventory is in sync with the avatar.
				gInventory.addChangedMask(LLInventoryObserver::LABEL, item_id);
				gInventory.notifyObservers();
			}
			return TRUE;
		}
		llwarns << "Not found" << llendl;
	}
	else
	{
		llwarns << "No item ID" << llendl;
	}
	return FALSE;
}
BOOL LLVOAvatarSelf::isWearingUnsupportedAttachment( const LLUUID& inv_item_id )
{
	std::map<S32, std::pair<LLUUID,LLUUID> >::iterator end = mUnsupportedAttachmentPoints.end();
	for(std::map<S32, std::pair<LLUUID,LLUUID> >::iterator iter = mUnsupportedAttachmentPoints.begin(); iter != end; ++iter)
	{
		if((*iter).second.first == inv_item_id)
		{
			return TRUE;
		}
	}
	return FALSE;
}
// </edit>
	
U32 LLVOAvatarSelf::getNumWearables(LLVOAvatarDefines::ETextureIndex i) const
{
	LLWearableType::EType type = LLVOAvatarDictionary::getInstance()->getTEWearableType(i);
	return gAgentWearables.getWearableCount(type);
}

// virtual
void LLVOAvatarSelf::localTextureLoaded(BOOL success, LLViewerFetchedTexture *src_vi, LLImageRaw* src_raw, LLImageRaw* aux_src, S32 discard_level, BOOL final, void* userdata)
{

	const LLUUID& src_id = src_vi->getID();
	LLAvatarTexData *data = (LLAvatarTexData *)userdata;
	ETextureIndex index = data->mIndex;
	if (!isIndexLocalTexture(index)) return;

	LocalTextureData &local_tex_data = mLocalTextureData[index];
	if (success)
	{
		if(!local_tex_data.mIsBakedReady &&
		   local_tex_data.mImage.notNull() &&
		   (local_tex_data.mImage->getID() == src_id) &&
		   discard_level < local_tex_data.mDiscard)
		{
			local_tex_data.mDiscard = discard_level;
			if ( isUsingBakedTextures() )
			{
				requestLayerSetUpdate( index );
			}
			else
			{
				LLVisualParamHint::requestHintUpdates();
			}
			updateMeshTextures();
		}
	}
	else if (final)
	{
			// Failed: asset is missing
		if(!local_tex_data.mIsBakedReady &&
		   local_tex_data.mImage.notNull() &&
		   local_tex_data.mImage->getID() == src_id)
		{
			local_tex_data.mDiscard = 0;
			requestLayerSetUpdate( index );
			updateMeshTextures();
		}
	}
}

// virtual
BOOL LLVOAvatarSelf::getLocalTextureGL(ETextureIndex type, LLViewerTexture** tex_pp) const
{
	*tex_pp = NULL;

	if (!isIndexLocalTexture(type)) return FALSE;
	if (getLocalTextureID(type) == IMG_DEFAULT_AVATAR) return TRUE;

	localtexture_map_t::const_iterator it = mLocalTextureData.find(type);
	if(it == mLocalTextureData.end())
		return FALSE;
	*tex_pp = it->second.mImage;
	return TRUE;
}

LLViewerFetchedTexture* LLVOAvatarSelf::getLocalTextureGL(LLVOAvatarDefines::ETextureIndex type) const
{
	if (!isIndexLocalTexture(type))
	{
		return NULL;
	}

	localtexture_map_t::const_iterator it = mLocalTextureData.find(type);
	if(it == mLocalTextureData.end() || it->second.mImage.isNull())
	{
		return NULL;
	}
	if (it->second.mImage->getID() == IMG_DEFAULT_AVATAR)
	{
		return LLViewerTextureManager::getFetchedTexture(IMG_DEFAULT_AVATAR);
	}
	return it->second.mImage;
}

const LLUUID& LLVOAvatarSelf::getLocalTextureID(ETextureIndex type) const
{
	if (!isIndexLocalTexture(type)) return IMG_DEFAULT_AVATAR;
	localtexture_map_t::const_iterator it = mLocalTextureData.find(type);
	if(it != mLocalTextureData.end() && it->second.mImage.notNull())
	{
		return it->second.mImage->getID();
	}
	return IMG_DEFAULT_AVATAR;
}


//-----------------------------------------------------------------------------
// isLocalTextureDataAvailable()
// Returns true if at least the lowest quality discard level exists for every texture
// in the layerset.
//-----------------------------------------------------------------------------
BOOL LLVOAvatarSelf::isLocalTextureDataAvailable( const LLTexLayerSet* layerset ) const
{
	/* if( layerset == mBakedTextureDatas[BAKED_HEAD].mTexLayerSet )
	   return getLocalDiscardLevel( TEX_HEAD_BODYPAINT ) >= 0; */
	for (LLVOAvatarDictionary::BakedTextures::const_iterator baked_iter = LLVOAvatarDictionary::getInstance()->getBakedTextures().begin();
		 baked_iter != LLVOAvatarDictionary::getInstance()->getBakedTextures().end();
		 ++baked_iter)
	{
		const EBakedTextureIndex baked_index = baked_iter->first;
		if (layerset == mBakedTextureDatas[baked_index].mTexLayerSet)
		{
			BOOL ret = true;
			const LLVOAvatarDictionary::BakedEntry *baked_dict = baked_iter->second;
			for (texture_vec_t::const_iterator local_tex_iter = baked_dict->mLocalTextures.begin();
				 local_tex_iter != baked_dict->mLocalTextures.end();
				 ++local_tex_iter)
			{
				ret &= (getLocalDiscardLevel(*local_tex_iter) >= 0);
			}
			return ret;
		}
	}
	llassert(0);
	return FALSE;
}

//-----------------------------------------------------------------------------
// isLocalTextureDataFinal()
// Returns true if the highest quality discard level exists for every texture
// in the layerset.
//-----------------------------------------------------------------------------
BOOL LLVOAvatarSelf::isLocalTextureDataFinal(const LLTexLayerSet* layerset) const
{
	for (U32 i = 0; i < mBakedTextureDatas.size(); i++)
	{
		if (layerset == mBakedTextureDatas[i].mTexLayerSet)
		{
			const LLVOAvatarDictionary::BakedEntry *baked_dict = LLVOAvatarDictionary::getInstance()->getBakedTexture((EBakedTextureIndex)i);
			for (texture_vec_t::const_iterator local_tex_iter = baked_dict->mLocalTextures.begin();
				 local_tex_iter != baked_dict->mLocalTextures.end();
				 ++local_tex_iter)
			{
				if (getLocalDiscardLevel(*local_tex_iter) != 0)
				{
					return FALSE;
				}
			}
			return TRUE;
		}
	}

	llassert(0);
	return FALSE;
}

BOOL LLVOAvatarSelf::isTextureDefined(LLVOAvatarDefines::ETextureIndex type) const
{
	return LLVOAvatar::isTextureDefined(type);
	/*LLUUID id;
	BOOL isDefined = TRUE;
	if (isIndexLocalTexture(type))
	{
		{
			id = getLocalTextureID(type);
			isDefined &= (id != IMG_DEFAULT_AVATAR && id != IMG_DEFAULT);
		}
	}
	else
	{
		id = getTEImage(type)->getID();
		isDefined &= (id != IMG_DEFAULT_AVATAR && id != IMG_DEFAULT);
	}
	
	return isDefined;*/
}

//virtual
BOOL LLVOAvatarSelf::isTextureVisible(LLVOAvatarDefines::ETextureIndex type) const
{
	return LLVOAvatar::isTextureVisible(type);
	/*
	if (isIndexBakedTexture(type))
	{
		return LLVOAvatar::isTextureVisible(type);
	}

	LLUUID tex_id = getLocalTextureID(type);
	return (tex_id != IMG_INVISIBLE) 
			|| (LLDrawPoolAlpha::sShowDebugAlpha);*/
}
//-----------------------------------------------------------------------------
// requestLayerSetUploads()
//-----------------------------------------------------------------------------
void LLVOAvatarSelf::requestLayerSetUploads()
{
	for (U32 i = 0; i < mBakedTextureDatas.size(); i++)
	{
		requestLayerSetUpload((EBakedTextureIndex)i);
	}
}

void LLVOAvatarSelf::requestLayerSetUpload(LLVOAvatarDefines::EBakedTextureIndex i)
{
	ETextureIndex tex_index = mBakedTextureDatas[i].mTextureIndex;
	const BOOL layer_baked = isTextureDefined(tex_index);
	if ( !layer_baked && mBakedTextureDatas[i].mTexLayerSet )
	{
		mBakedTextureDatas[i].mTexLayerSet->requestUpload();
	}
}

bool LLVOAvatarSelf::areTexturesCurrent() const
{
	return !hasPendingBakedUploads() && gAgentWearables.areWearablesLoaded();
}

// virtual
bool LLVOAvatarSelf::hasPendingBakedUploads() const
{
	for (U32 i = 0; i < mBakedTextureDatas.size(); i++)
	{
		LLTexLayerSet* layerset = mBakedTextureDatas[i].mTexLayerSet;
		if (layerset && layerset->getComposite() && layerset->getComposite()->uploadPending())
		{
			return true;
		}
	}
	return false;
}

void LLVOAvatarSelf::invalidateComposite( LLTexLayerSet* layerset, BOOL upload_result )
{
	if( !layerset || !layerset->getUpdatesEnabled() )
	{
		return;
	}
	// llinfos << "LLVOAvatar::invalidComposite() " << layerset->getBodyRegionName() << llendl;

	layerset->requestUpdate();

	if( upload_result )
	{
		llassert( isSelf() );

		ETextureIndex baked_te = getBakedTE( layerset );
		setTEImage( baked_te, LLViewerTextureManager::getFetchedTexture(IMG_DEFAULT_AVATAR,0));
		layerset->requestUpload();
		updateMeshTextures();
	}
}

void LLVOAvatarSelf::invalidateAll()
{
	for (U32 i = 0; i < mBakedTextureDatas.size(); i++)
	{
		invalidateComposite(mBakedTextureDatas[i].mTexLayerSet, TRUE);
	}
}

//-----------------------------------------------------------------------------
// setCompositeUpdatesEnabled()
//-----------------------------------------------------------------------------
void LLVOAvatarSelf::setCompositeUpdatesEnabled( bool b )
{
	for (U32 i = 0; i < mBakedTextureDatas.size(); i++)
	{
		setCompositeUpdatesEnabled(i, b);
	}
}

void LLVOAvatarSelf::setCompositeUpdatesEnabled(U32 index, bool b)
{
	if (mBakedTextureDatas[index].mTexLayerSet )
	{
		mBakedTextureDatas[index].mTexLayerSet->setUpdatesEnabled( b );
	}
}

bool LLVOAvatarSelf::isCompositeUpdateEnabled(U32 index)
{
	if (mBakedTextureDatas[index].mTexLayerSet)
	{
		return mBakedTextureDatas[index].mTexLayerSet->getUpdatesEnabled();
	}
	return false;
}

void LLVOAvatarSelf::setupComposites()
{
	for (U32 i = 0; i < mBakedTextureDatas.size(); i++)
	{
		ETextureIndex tex_index = mBakedTextureDatas[i].mTextureIndex;
		BOOL layer_baked = isTextureDefined(tex_index);
		if (mBakedTextureDatas[i].mTexLayerSet)
		{
			mBakedTextureDatas[i].mTexLayerSet->setUpdatesEnabled( !layer_baked );
		}
	}
}

void LLVOAvatarSelf::updateComposites()
{
	for (U32 i = 0; i < mBakedTextureDatas.size(); i++)
	{
		if ( mBakedTextureDatas[i].mTexLayerSet
			&& ((i != BAKED_SKIRT) || isWearingWearableType( LLWearableType::WT_SKIRT )) )
		{
			mBakedTextureDatas[i].mTexLayerSet->updateComposite();
		}
	}
}

// virtual
S32 LLVOAvatarSelf::getLocalDiscardLevel( ETextureIndex type ) const
{
	// If the texture is not local, we don't care and treat it as fully loaded
	if (!isIndexLocalTexture(type)) return FALSE;

	localtexture_map_t::const_iterator it = mLocalTextureData.find(type);
	if (type >= 0
		&& it != mLocalTextureData.end()
		&& getLocalTextureID(type) != IMG_DEFAULT_AVATAR
		&& !it->second.mImage->isMissingAsset())
	{
		return it->second.mImage->getDiscardLevel();
	}
	else
	{
		// We don't care about this (no image associated with the layer) treat as fully loaded.
		return 0;
	}
}
// Counts the memory footprint of local textures.
void LLVOAvatarSelf::getLocalTextureByteCount( S32* gl_bytes ) const
{
	*gl_bytes = 0;
	for( S32 type = 0; type < TEX_NUM_INDICES; type++ )
	{
		if (!isIndexLocalTexture((ETextureIndex)type)) continue;
		localtexture_map_t::const_iterator it = mLocalTextureData.find((ETextureIndex)type);
		if(it == mLocalTextureData.end())continue;
		LLViewerTexture* image_gl = it->second.mImage;
		if( image_gl )
		{
			S32 bytes = (S32)image_gl->getWidth() * image_gl->getHeight() * image_gl->getComponents();

			if( image_gl->hasGLTexture() )
			{
				*gl_bytes += bytes;
			}
		}
	}
}
// virtual 
void LLVOAvatarSelf::setLocalTexture( ETextureIndex type, LLViewerTexture* src_tex, BOOL baked_version_ready )
{
	if (!isIndexLocalTexture(type)) return;

	LLViewerFetchedTexture* tex = LLViewerTextureManager::staticCastToFetchedTexture(src_tex, TRUE) ;
	if(!tex)
	{
		return ;
	}
	
	S32 desired_discard = isSelf() ? 0 : 2;
	LocalTextureData &local_tex_data = mLocalTextureData[type];
	if (!baked_version_ready)
	{
		if (tex != local_tex_data.mImage || local_tex_data.mIsBakedReady)
		{
			local_tex_data.mDiscard = MAX_DISCARD_LEVEL+1;
		}
		if (tex->getID() != IMG_DEFAULT_AVATAR)
		{
			if (local_tex_data.mDiscard > desired_discard)
			{
				S32 tex_discard = tex->getDiscardLevel();
				if (tex_discard >= 0 && tex_discard <= desired_discard)
				{
					local_tex_data.mDiscard = tex_discard;
					if (gAgentAvatarp->isUsingBakedTextures())
					{
						requestLayerSetUpdate( type );
					}
					else
					{
						LLVisualParamHint::requestHintUpdates();
					}
				}
				else
				{
					tex->setLoadedCallback( onLocalTextureLoaded, desired_discard, TRUE, FALSE, new LLAvatarTexData(getID(),type), NULL );
				}
			}
			tex->setMinDiscardLevel(desired_discard);
		}
	}
	local_tex_data.mIsBakedReady = baked_version_ready;
	local_tex_data.mImage = tex;
}

//virtual
void LLVOAvatarSelf::dumpLocalTextures() const
{
	llinfos << "Local Textures:" << llendl;

	/* ETextureIndex baked_equiv[] = {
		TEX_UPPER_BAKED,
	   if (isTextureDefined(baked_equiv[i])) */
	for (LLVOAvatarDictionary::Textures::const_iterator iter = LLVOAvatarDictionary::getInstance()->getTextures().begin();
		 iter != LLVOAvatarDictionary::getInstance()->getTextures().end();
		 iter++)
	{
		const LLVOAvatarDictionary::TextureEntry *text_dict = iter->second;
		if (!text_dict->mIsLocalTexture || !text_dict->mIsUsedByBakedTexture)
			continue;

		const EBakedTextureIndex baked_index = text_dict->mBakedTextureIndex;
		const ETextureIndex baked_equiv = LLVOAvatarDictionary::getInstance()->getBakedTexture(baked_index)->mTextureIndex;

		const std::string &name = text_dict->mName;
		localtexture_map_t::const_iterator it = mLocalTextureData.find(iter->first);
		if (isTextureDefined(baked_equiv))
		{
#if LL_RELEASE_FOR_DOWNLOAD
			// End users don't get to trivially see avatar texture IDs, makes textures
			// easier to steal. JC
			llinfos << "LocTex " << name << ": Baked " << llendl;
#else
			llinfos << "LocTex " << name << ": Baked " << getTEImage( baked_equiv )->getID() << llendl;
#endif
		}
		else if (it != mLocalTextureData.end() && it->second.mImage.notNull())
		{
			if( it->second.mImage->getID() == IMG_DEFAULT_AVATAR )
			{
				llinfos << "LocTex " << name << ": None" << llendl;
			}
			else
			{
				const LLViewerFetchedTexture* image = it->second.mImage;

				llinfos << "LocTex " << name << ": "
						<< "Discard " << image->getDiscardLevel() << ", "
						<< "(" << image->getWidth() << ", " << image->getHeight() << ") "
#if !LL_RELEASE_FOR_DOWNLOAD
					// End users don't get to trivially see avatar texture IDs,
					// makes textures easier to steal
						<< image->getID() << " "
#endif
						<< "Priority: " << image->getDecodePriority()
						<< llendl;
			}
		}
		else
		{
			llinfos << "LocTex " << name << ": No LLViewerTexture" << llendl;
		}
	}
}

//-----------------------------------------------------------------------------
// static 
// onLocalTextureLoaded()
//-----------------------------------------------------------------------------

void LLVOAvatarSelf::onLocalTextureLoaded( BOOL success, LLViewerFetchedTexture *src_vi, LLImageRaw* src_raw, LLImageRaw* aux_src, S32 discard_level, BOOL final, void* userdata )
{
	LLAvatarTexData *data = (LLAvatarTexData *)userdata;
	LLVOAvatarSelf *self = (LLVOAvatarSelf *)gObjectList.findAvatar(data->mAvatarID);
	if (self)
	{
		// We should only be handling local textures for ourself
		self->localTextureLoaded(success, src_vi, src_raw, aux_src, discard_level, final, userdata);
	}
	// ensure data is cleaned up
	if (final || !success)
	{
		delete data;
	}
}

/*virtual*/	void LLVOAvatarSelf::setImage(const U8 te, LLViewerTexture *imagep)
{
	LLVOAvatar::setImage(te,imagep);
	/*if (isIndexLocalTexture((ETextureIndex)te))
	{
		setLocalTexture((ETextureIndex)te, imagep, FALSE);
	}
	else 
	{
		setTEImage(te,imagep);
	}*/
}

/*virtual*/ LLViewerTexture* LLVOAvatarSelf::getImage(const U8 te) const
{
	return LLVOAvatar::getImage(te);
	/*if (isIndexLocalTexture((ETextureIndex)te))
	{
		return getLocalTextureGL((ETextureIndex)te);
	}
	else 
	{
		return getTEImage(te);
	}*/
}

// static
void LLVOAvatarSelf::dumpTotalLocalTextureByteCount()
{
	S32 gl_bytes = 0;
	gAgentAvatarp->getLocalTextureByteCount(&gl_bytes);
	llinfos << "Total Avatar LocTex GL:" << (gl_bytes/1024) << "KB" << llendl;
}

BOOL LLVOAvatarSelf::getIsCloud()
{
	// do we have a shape?
	if (gAgentWearables.getWearableCount(LLWearableType::WT_SHAPE) == 0 ||
		gAgentWearables.getWearableCount(LLWearableType::WT_HAIR) == 0 ||
		gAgentWearables.getWearableCount(LLWearableType::WT_EYES) == 0 ||
		gAgentWearables.getWearableCount(LLWearableType::WT_SKIN) == 0)	
	{
		return TRUE;
	}

	if (!isTextureDefined(TEX_HAIR))
	{
		return TRUE;
	}

	if (!mPreviousFullyLoaded)
	{
		if (!isLocalTextureDataAvailable(mBakedTextureDatas[BAKED_LOWER].mTexLayerSet) &&
			(!isTextureDefined(TEX_LOWER_BAKED)))
		{
			return TRUE;
		}

		if (!isLocalTextureDataAvailable(mBakedTextureDatas[BAKED_UPPER].mTexLayerSet) &&
			(!isTextureDefined(TEX_UPPER_BAKED)))
		{
			return TRUE;
		}
	}
	return FALSE;
}

const LLUUID& LLVOAvatarSelf::grabBakedTexture(EBakedTextureIndex baked_index) const
{
	if (canGrabBakedTexture(baked_index))
	{
		ETextureIndex tex_index = LLVOAvatarDictionary::bakedToLocalTextureIndex(baked_index);
		if (tex_index == TEX_NUM_INDICES)
		{
			return LLUUID::null;
		}
		return getTEImage( tex_index )->getID();
	}
	return LLUUID::null;
}

BOOL LLVOAvatarSelf::canGrabBakedTexture(EBakedTextureIndex baked_index) const
{
	ETextureIndex tex_index = LLVOAvatarDictionary::bakedToLocalTextureIndex(baked_index);
	if (tex_index == TEX_NUM_INDICES)
	{
		return FALSE;
	}
	// Check if the texture hasn't been baked yet.
	if (!isTextureDefined(tex_index))
	{
		lldebugs << "getTEImage( " << (U32) tex_index << " )->getID() == IMG_DEFAULT_AVATAR" << llendl;
		return FALSE;
	}

	if (gAgent.isGodlikeWithoutAdminMenuFakery())
		return TRUE;

	// Check permissions of textures that show up in the
	// baked texture.  We don't want people copying people's
	// work via baked textures.


	const LLVOAvatarDictionary::BakedEntry *baked_dict = LLVOAvatarDictionary::getInstance()->getBakedTexture(baked_index);
	for (texture_vec_t::const_iterator iter = baked_dict->mLocalTextures.begin();
		 iter != baked_dict->mLocalTextures.end();
		 ++iter)
	{
		const ETextureIndex t_index = (*iter);
		lldebugs << "Checking index " << (U32) t_index << llendl;
		const LLUUID& texture_id = getTEImage( t_index )->getID();
		if (texture_id != IMG_DEFAULT_AVATAR)
		{
			// Search inventory for this texture.
			LLViewerInventoryCategory::cat_array_t cats;
			LLViewerInventoryItem::item_array_t items;
			LLAssetIDMatches asset_id_matches(texture_id);
			gInventory.collectDescendentsIf(LLUUID::null,
									cats,
									items,
									LLInventoryModel::INCLUDE_TRASH,
									asset_id_matches);

			BOOL can_grab = FALSE;
			lldebugs << "item count for asset " << texture_id << ": " << items.count() << llendl;
			if (items.count())
			{
				// search for full permissions version
				for (S32 i = 0; i < items.count(); i++)
				{
					LLViewerInventoryItem* itemp = items[i];
					if (itemp->getIsFullPerm())
					{
						can_grab = TRUE;
						break;
					}
				}
			}
			if (!can_grab) return FALSE;
		}
	}

	return TRUE;
}
void LLVOAvatarSelf::addLocalTextureStats( ETextureIndex type, LLViewerFetchedTexture* imagep,
									   F32 texel_area_ratio, BOOL render_avatar, BOOL covered_by_baked )
{
	if (!isIndexLocalTexture(type)) return;

	if (!covered_by_baked)
	{
		if (getLocalTextureID(type) != IMG_DEFAULT_AVATAR && imagep->getDiscardLevel() != 0)
		{
			F32 desired_pixels;
			desired_pixels = llmin(mPixelArea, (F32)getTexImageArea());
			imagep->setBoostLevel(getAvatarBoostLevel());

			imagep->resetTextureStats();
			imagep->setMaxVirtualSizeResetInterval(MAX_TEXTURE_VIRTURE_SIZE_RESET_INTERVAL);
			imagep->addTextureStats( desired_pixels / texel_area_ratio );
			imagep->setAdditionalDecodePriority(SELF_ADDITIONAL_PRI) ;
			imagep->forceUpdateBindStats() ;
			if (imagep->getDiscardLevel() < 0)
			{
				mHasGrey = TRUE; // for statistics gathering
			}
		}
		else
		{
			// texture asset is missing
			mHasGrey = TRUE; // for statistics gathering
		}
	}
}

//-----------------------------------------------------------------------------
// getBakedTE()
// Used by the LayerSet.  (Layer sets don't in general know what textures depend on them.)
//-----------------------------------------------------------------------------
ETextureIndex LLVOAvatarSelf::getBakedTE(const LLTexLayerSet* layerset ) const
{
	for (U32 i = 0; i < mBakedTextureDatas.size(); i++)
	{
		if (layerset == mBakedTextureDatas[i].mTexLayerSet )
		{
			return mBakedTextureDatas[i].mTextureIndex;
		}
	}
	llassert(0);
	return TEX_HEAD_BAKED;
}


void LLVOAvatarSelf::setNewBakedTexture(LLVOAvatarDefines::EBakedTextureIndex i, const LLUUID &uuid)
{
	ETextureIndex index = LLVOAvatarDictionary::bakedToLocalTextureIndex(i);
	setNewBakedTexture(index, uuid);
}

//-----------------------------------------------------------------------------
// setNewBakedTexture()
// A new baked texture has been successfully uploaded and we can start using it now.
//-----------------------------------------------------------------------------
void LLVOAvatarSelf::setNewBakedTexture( ETextureIndex te, const LLUUID& uuid )
{
	// Baked textures live on other sims.
	LLHost target_host = getObjectHost();
	setTEImage( te, LLViewerTextureManager::getFetchedTextureFromHost( uuid, target_host ) );
	if (uuid != IMG_INVISIBLE)
	{
		// Do not update textures when setting a new invisible baked texture as
		// it would result in destroying the calling object (setNewBakedTexture()
		// is called by LLTexLayerSetBuffer::render()) !
		updateMeshTextures();
	}
	dirtyMesh();


	LLVOAvatar::cullAvatarsByPixelArea();

	/* switch(te)
		case TEX_HEAD_BAKED:
			llinfos << "New baked texture: HEAD" << llendl; */
	const LLVOAvatarDictionary::TextureEntry *texture_dict = LLVOAvatarDictionary::getInstance()->getTexture(te);
	if (texture_dict->mIsBakedTexture)
	{
		llinfos << "New baked texture: " << texture_dict->mName << " UUID: " << uuid <<llendl;
		//mBakedTextureDatas[text_dict->mBakedTextureIndex].mTexLayerSet->requestUpdate();
	}
	else
	{
		llwarns << "New baked texture: unknown te " << te << llendl;
	}
	
	//	dumpAvatarTEs( "setNewBakedTexture() send" );
	// RN: throttle uploads
	if (!hasPendingBakedUploads())
	{
		gAgent.sendAgentSetAppearance();
	}
}
//-----------------------------------------------------------------------------
// setCachedBakedTexture()
// A baked texture id was received from a cache query, make it active
//-----------------------------------------------------------------------------
void LLVOAvatarSelf::setCachedBakedTexture( ETextureIndex te, const LLUUID& uuid )
{
	setTETexture( te, uuid );

	/* switch(te)
		case TEX_HEAD_BAKED:
			if( mHeadLayerSet )
				mHeadLayerSet->cancelUpload(); */
	for (U32 i = 0; i < mBakedTextureDatas.size(); i++)
	{
		if ( mBakedTextureDatas[i].mTextureIndex == te && mBakedTextureDatas[i].mTexLayerSet)
		{
			mBakedTextureDatas[i].mTexLayerSet->cancelUpload();
		}
	}
}
// static
void LLVOAvatarSelf::processRebakeAvatarTextures(LLMessageSystem* msg, void**)
{
	LLUUID texture_id;
	msg->getUUID("TextureData", "TextureID", texture_id);
	if (!isAgentAvatarValid()) return;

	// If this is a texture corresponding to one of our baked entries, 
	// just rebake that layer set.
	BOOL found = FALSE;

	/* ETextureIndex baked_texture_indices[BAKED_NUM_INDICES] =
			TEX_HEAD_BAKED,
			TEX_UPPER_BAKED, */
	for (LLVOAvatarDictionary::Textures::const_iterator iter = LLVOAvatarDictionary::getInstance()->getTextures().begin();
		 iter != LLVOAvatarDictionary::getInstance()->getTextures().end();
		 ++iter)
	{
		const ETextureIndex index = iter->first;
		const LLVOAvatarDictionary::TextureEntry *texture_dict = iter->second;
		if (texture_dict->mIsBakedTexture)
		{
			if (texture_id == gAgentAvatarp->getTEImage(index)->getID())
			{
				LLTexLayerSet* layer_set = gAgentAvatarp->getLayerSet(index);
				if (layer_set)
				{
					llinfos << "TAT: rebake - matched entry " << (S32)index << llendl;
					gAgentAvatarp->invalidateComposite(layer_set, TRUE);
					found = TRUE;
					LLViewerStats::getInstance()->incStat(LLViewerStats::ST_TEX_REBAKES);
				}
			}
		}
	}

	// If texture not found, rebake all entries.
	if (!found)
	{
		gAgentAvatarp->forceBakeAllTextures();
	}
	else
	{
		// Not sure if this is necessary, but forceBakeAllTextures() does it.
		gAgentAvatarp->updateMeshTextures();
	}
}

BOOL LLVOAvatarSelf::isUsingBakedTextures() const
{
	// Composite textures are used during appearance mode.
	if (gAgentCamera.cameraCustomizeAvatar())
		return FALSE;

	return TRUE;
}


void LLVOAvatarSelf::forceBakeAllTextures(bool slam_for_debug)
{
	llinfos << "TAT: forced full rebake. " << llendl;

	for (U32 i = 0; i < mBakedTextureDatas.size(); i++)
	{
		ETextureIndex baked_index = mBakedTextureDatas[i].mTextureIndex;
		LLTexLayerSet* layer_set = getLayerSet(baked_index);
		if (layer_set)
		{
			if (slam_for_debug)
			{
				layer_set->setUpdatesEnabled(TRUE);
				layer_set->cancelUpload();
			}

			invalidateComposite(layer_set, TRUE);
			LLViewerStats::getInstance()->incStat(LLViewerStats::ST_TEX_REBAKES);
		}
		else
		{
			llwarns << "TAT: NO LAYER SET FOR " << (S32)baked_index << llendl;
		}
	}

	// Don't know if this is needed
	updateMeshTextures();
}

//-----------------------------------------------------------------------------
// requestLayerSetUpdate()
//-----------------------------------------------------------------------------
void LLVOAvatarSelf::requestLayerSetUpdate(ETextureIndex index )
{
	/* switch(index)
		case LOCTEX_UPPER_BODYPAINT:  
		case LOCTEX_UPPER_SHIRT:
			if( mUpperBodyLayerSet )
				mUpperBodyLayerSet->requestUpdate(); */
	const LLVOAvatarDictionary::TextureEntry *texture_dict = LLVOAvatarDictionary::getInstance()->getTexture(index);
	if (!texture_dict->mIsLocalTexture || !texture_dict->mIsUsedByBakedTexture)
		return;
	const EBakedTextureIndex baked_index = texture_dict->mBakedTextureIndex;
	if (mBakedTextureDatas[baked_index].mTexLayerSet)
	{
		mBakedTextureDatas[baked_index].mTexLayerSet->requestUpdate();
	}
}

LLTexLayerSet* LLVOAvatarSelf::getLayerSet(ETextureIndex index) const
{
	/* switch(index)
		case TEX_HEAD_BAKED:
		case TEX_HEAD_BODYPAINT:
			return mHeadLayerSet; */
	const LLVOAvatarDictionary::TextureEntry *texture_dict = LLVOAvatarDictionary::getInstance()->getTexture(index);
	if (texture_dict->mIsUsedByBakedTexture)
	{
		const EBakedTextureIndex baked_index = texture_dict->mBakedTextureIndex;
		return mBakedTextureDatas[baked_index].mTexLayerSet;
	}
	return NULL;
}

// static
void LLVOAvatarSelf::onCustomizeStart()
{
	// We're no longer doing any baking or invalidating on entering 
	// appearance editing mode. Leaving function in place in case 
	// further changes require us to do something at this point - Nyx
}

// static
void LLVOAvatarSelf::onCustomizeEnd()
{
	if (isAgentAvatarValid())
	{
		gAgentAvatarp->invalidateAll();
		gAgentAvatarp->requestLayerSetUploads();
	}
}

// HACK: this will null out the avatar's local texture IDs before the TE message is sent
//       to ensure local texture IDs are not sent to other clients in the area.
//       this is a short-term solution. The long term solution will be to not set the texture
//       IDs in the avatar object, and keep them only in the wearable.
//       This will involve further refactoring that is too risky for the initial release of 2.0.
bool LLVOAvatarSelf::sendAppearanceMessage(LLMessageSystem *mesgsys)// const
{
	LLUUID texture_id[TEX_NUM_INDICES];
	// pack away current TEs to make sure we don't send them out
	for (LLVOAvatarDictionary::Textures::const_iterator iter = LLVOAvatarDictionary::getInstance()->getTextures().begin();
		 iter != LLVOAvatarDictionary::getInstance()->getTextures().end();
		 ++iter)
	{
		const ETextureIndex index = iter->first;
		const LLVOAvatarDictionary::TextureEntry *texture_dict = iter->second;
		if (!texture_dict->mIsBakedTexture)
		{
			LLTextureEntry* entry = getTE((U8) index);
			texture_id[index] = entry->getID();
			entry->setID(IMG_DEFAULT_AVATAR);
		}
	}

	/*if (gSavedSettings.getBOOL("AscentUseCustomTag"))
		{
			LLColor4 color;
			if (!gSavedSettings.getBOOL("AscentStoreSettingsPerAccount"))
			{
				color = gSavedSettings.setColor4("AscentCustomTagColor");
			}
			else
			{
				color = gSavedPerAccountSettings.getColor4("AscentCustomTagColor");
			}
			LLUUID old_teid;
			U8 client_buffer[UUID_BYTES];
			memset(&client_buffer, 0, UUID_BYTES);
			LLTextureEntry* entry = (LLTextureEntry*)gAgentAvatarp->getTE(0);
			old_teid = entry->getID();
			//You edit this to change the tag in your client. Yes.
			const char* tag_client = "Ascent";
			strncpy((char*)&client_buffer[0], tag_client, UUID_BYTES);
			LLUUID part_a;
			memcpy(&part_a.mData, &client_buffer[0], UUID_BYTES);
			entry->setColor(color);
			//This glow is used to tell if the tag color and name is set or not.
			entry->setGlow(0.1f);
			entry->setID(part_a);
			gAgentAvatarp->packTEMessage( gMessageSystem, 1, gSavedSettings.getString("AscentReportClientUUID") );
			entry->setID(old_teid);
			
		}
*/
		
	std::string client_tag;
	if(gSavedSettings.getBOOL("AscentUseTag"))
		client_tag = gSavedSettings.getString("AscentReportClientUUID");
	else
		client_tag = "c228d1cf-4b5d-4ba8-84f4-899a0796aa97";
		
	bool success = packTEMessage(mesgsys, 1, client_tag);

	// unpack TEs to make sure we don't re-trigger a bake
	for (LLVOAvatarDictionary::Textures::const_iterator iter = LLVOAvatarDictionary::getInstance()->getTextures().begin();
		 iter != LLVOAvatarDictionary::getInstance()->getTextures().end();
		 ++iter)
	{
		const ETextureIndex index = iter->first;
		const LLVOAvatarDictionary::TextureEntry *texture_dict = iter->second;
		if (!texture_dict->mIsBakedTexture)
		{
			LLTextureEntry* entry = getTE((U8) index);
			entry->setID(texture_id[index]);
		}
	}

	clearClientTag();
	return success;
}


//------------------------------------------------------------------------
// needsRenderBeam()
//------------------------------------------------------------------------
BOOL LLVOAvatarSelf::needsRenderBeam()
{
	if (gNoRender)
	{
		return FALSE;
	}

	LLTool *tool = LLToolMgr::getInstance()->getCurrentTool();

	BOOL is_touching_or_grabbing = (tool == LLToolGrab::getInstance() && LLToolGrab::getInstance()->isEditing());
	if (LLToolGrab::getInstance()->getEditingObject() && 
		LLToolGrab::getInstance()->getEditingObject()->isAttachment())
	{
		// don't render selection beam on hud objects
		is_touching_or_grabbing = FALSE;
	}
	return is_touching_or_grabbing || (mState & AGENT_STATE_EDITING && LLSelectMgr::getInstance()->shouldShowSelection());
}

// static
void LLVOAvatarSelf::deleteScratchTextures()
{
if(gAuditTexture)
	{
		S32 total_tex_size = sScratchTexBytes ;
		S32 tex_size = SCRATCH_TEX_WIDTH * SCRATCH_TEX_HEIGHT ;

		if( sScratchTexNames.checkData( GL_LUMINANCE ) )
		{
			LLImageGL::decTextureCounter(tex_size, 1, LLViewerTexture::AVATAR_SCRATCH_TEX) ;
			total_tex_size -= tex_size ;
		}
		if( sScratchTexNames.checkData( GL_ALPHA ) )
		{
			LLImageGL::decTextureCounter(tex_size, 1, LLViewerTexture::AVATAR_SCRATCH_TEX) ;
			total_tex_size -= tex_size ;
		}
		if( sScratchTexNames.checkData( GL_COLOR_INDEX ) )
		{
			LLImageGL::decTextureCounter(tex_size, 1, LLViewerTexture::AVATAR_SCRATCH_TEX) ;
			total_tex_size -= tex_size ;
		}
		if( sScratchTexNames.checkData( GL_LUMINANCE_ALPHA ) )
		{
			LLImageGL::decTextureCounter(tex_size, 2, LLViewerTexture::AVATAR_SCRATCH_TEX) ;
			total_tex_size -= 2 * tex_size ;
		}
		if( sScratchTexNames.checkData( GL_RGB ) )
		{
			LLImageGL::decTextureCounter(tex_size, 3, LLViewerTexture::AVATAR_SCRATCH_TEX) ;
			total_tex_size -= 3 * tex_size ;
		}
		if( sScratchTexNames.checkData( GL_RGBA ) )
		{
			LLImageGL::decTextureCounter(tex_size, 4, LLViewerTexture::AVATAR_SCRATCH_TEX) ;
			total_tex_size -= 4 * tex_size ;
		}
		//others
		while(total_tex_size > 0)
		{
			LLImageGL::decTextureCounter(tex_size, 4, LLViewerTexture::AVATAR_SCRATCH_TEX) ;
			total_tex_size -= 4 * tex_size ;
		}
	}

	for( LLGLuint* namep = sScratchTexNames.getFirstData();
		 namep;
		 namep = sScratchTexNames.getNextData() )
	{
		LLImageGL::deleteTextures(1, (U32 *)namep );
		stop_glerror();
	}

	if( sScratchTexBytes )
	{
		lldebugs << "Clearing Scratch Textures " << (sScratchTexBytes/1024) << "KB" << llendl;

		sScratchTexNames.deleteAllData();
		sScratchTexLastBindTime.deleteAllData();
		LLImageGL::sGlobalTextureMemoryInBytes -= sScratchTexBytes;
		sScratchTexBytes = 0;
	}
}

// static 
void LLVOAvatarSelf::dumpScratchTextureByteCount()
{
	llinfos << "Scratch Texture GL: " << (sScratchTexBytes/1024) << "KB" << llendl;
}

//static
void LLVOAvatarSelf::onChangeSelfInvisible(BOOL newvalue)
{
	if (isAgentAvatarValid())
	{
		if (newvalue)
		{
			// we have just requested to set the avatar's baked textures to invisible
			gAgentAvatarp->setInvisible(TRUE);
		}
		else
		{
			gAgentAvatarp->setInvisible(FALSE);
		}
	}
}


void LLVOAvatarSelf::setInvisible(BOOL newvalue)
{
	if (newvalue)
	{
		setCompositeUpdatesEnabled(FALSE);
		for (U32 i = 0; i < mBakedTextureDatas.size(); i++ )
		{
			setNewBakedTexture(mBakedTextureDatas[i].mTextureIndex, IMG_INVISIBLE);
		}
		gAgent.sendAgentSetAppearance();
	}
	else
	{
		setCompositeUpdatesEnabled(TRUE);
		invalidateAll();
		requestLayerSetUploads();
		gAgent.sendAgentSetAppearance();
	}
}