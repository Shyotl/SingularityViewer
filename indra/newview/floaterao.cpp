/** 
 * @file llfloaterao.cpp
 * @brief clientside animation overrider
 * by Skills Hak
 */

#include "llviewerprecompiledheaders.h"

#include "floaterao.h"

#include "llagent.h"
#include "llagentcamera.h"
#include "llvoavatarself.h"
#include "llanimationstates.h"
#include "lluictrlfactory.h"
#include "llstartup.h"
#include "llpreviewnotecard.h"
#include "llviewertexteditor.h"
#include "llcombobox.h"
#include "llfirstuse.h"

#include "llinventory.h"
#include "llinventoryfunctions.h"
#include "llinventorypanel.h"
#include "llinventorymodelbackgroundfetch.h"
#include "roles_constants.h"
#include "llviewerregion.h"

#include "llinventorybridge.h"

#include <boost/regex.hpp>

// Uncomment and use instead if we ever add the chatbar as a command line - MC
void cmdline_printchat(const std::string& message);

namespace
{
	bool sSwimming = false;
	bool enable_swim()
	{
		static const LLCachedControl<bool> swim(gSavedSettings, "AOSwimEnabled", false);
		return swim;
	}
	bool is_underwater() { return enable_swim() && gAgentAvatarp && gAgentAvatarp->mBelowWater; }
}

class AONotecardCallback : public LLInventoryCallback
{
public:
	AONotecardCallback(std::string &filename)
	{
		mFileName = filename;
	}

	void fire(const LLUUID &inv_item)
	{
		if (!mFileName.empty())
		{ 
			LLPreviewNotecard* nc = (LLPreviewNotecard*)LLPreview::show(inv_item);
			if (!nc)
			{
				auto item = gInventory.getItem(inv_item);
				open_notecard(item, "Note: " + item->getName(), LLUUID::null, false);
				nc = (LLPreviewNotecard*)LLPreview::find(inv_item);
			}

			if (nc)
			{
				if (LLTextEditor *text = nc->getEditor())
				{
					text->clear();
					text->makePristine();

					std::ifstream file(mFileName.c_str());

					std::string line;
					while (!file.eof())
					{
						getline(file, line);
						line += '\n';
						text->insertText(line);
					}
					file.close();

					nc->saveIfNeeded();
				}
			}
		}
	}

private:
	std::string mFileName;
};

AOInvTimer* gAOInvTimer = nullptr;


// -------------------------------------------------------

AOStandTimer* mAOStandTimer;

AOStandTimer::AOStandTimer() : LLEventTimer( gSavedSettings.getF32("AOStandInterval") )
{
	tick();
}
AOStandTimer::~AOStandTimer()
{
//	LL_INFOS() << "dead" << LL_ENDL;
}
void AOStandTimer::reset()
{
	mPeriod = gSavedSettings.getF32("AOStandInterval");
	mEventTimer.reset();
//	LL_INFOS() << "reset" << LL_ENDL;
}
BOOL AOStandTimer::tick()
{
	LLFloaterAO::stand_iterator++;
//	LL_INFOS() << "tick" << LL_ENDL;
	LLFloaterAO::ChangeStand();
	return false;
//	return LLFloaterAO::ChangeStand(); //timer is always active now ..
}

// -------------------------------------------------------

AOInvTimer::AOInvTimer() : LLEventTimer( 1.0f )
{
}
AOInvTimer::~AOInvTimer()
{
}
BOOL AOInvTimer::tick()
{
	if(LLStartUp::getStartupState() >= STATE_INVENTORY_SEND)
	{
		if(LLInventoryModelBackgroundFetch::instance().isEverythingFetched())
		{
//			cmdline_printchat("Inventory fetched, loading AO.");
			LLFloaterAO::getInstance(); // Initializes everything
			return true;
		}
	}
	return !gSavedSettings.getBOOL("AOEnabled");
}

// STUFF -------------------------------------------------------

AOState LLFloaterAO::mAnimationState = STATE_AGENT_IDLE;
int LLFloaterAO::stand_iterator = 0;

LLUUID LLFloaterAO::invfolderid = LLUUID::null;
LLUUID LLFloaterAO::mCurrentStandId = LLUUID::null;

LLComboBox* mcomboBox_stands;
LLComboBox* mcomboBox_walks;
LLComboBox* mcomboBox_runs;
LLComboBox* mcomboBox_jumps;
LLComboBox* mcomboBox_sits;
LLComboBox* mcomboBox_gsits;
LLComboBox* mcomboBox_crouchs;
LLComboBox* mcomboBox_cwalks;
LLComboBox* mcomboBox_falls;
LLComboBox* mcomboBox_hovers;
LLComboBox* mcomboBox_flys;
LLComboBox* mcomboBox_flyslows;
LLComboBox* mcomboBox_flyups;
LLComboBox* mcomboBox_flydowns;
LLComboBox* mcomboBox_lands;
LLComboBox* mcomboBox_typings;
LLComboBox* mcomboBox_floats;
LLComboBox* mcomboBox_swims;
LLComboBox* mcomboBox_swimups;
LLComboBox* mcomboBox_swimdowns;
LLComboBox* mcomboBox_standups;
LLComboBox* mcomboBox_prejumps;

struct struct_overrides
{
	LLUUID orig_id;
	LLUUID ao_id = LLUUID::null;
	AOState state;
};
std::vector<struct_overrides> mAOOverrides;

struct struct_stands
{
	LLUUID ao_id;
	std::string anim_name;
};
std::vector<struct_stands> mAOStands;

LLFloaterAO::LLFloaterAO(const LLSD&) : LLFloater("floater_ao")
{
	LLUICtrlFactory::getInstance()->buildFloater(this, "floater_ao.xml", nullptr, false);
	init();
	gSavedSettings.getControl("AOEnabled")->getSignal()->connect(boost::bind(&LLFloaterAO::run));
	gSavedSettings.getControl("AOEnabled")->getSignal()->connect(std::bind([](const LLSD& enabled)
	{
		// Toggle typing AO the moment we toggle AO
		const bool typing = gAgent.getRenderState() & AGENT_STATE_TYPING;
		gAgent.sendAnimationRequest(GetAnimIDFromState(STATE_AGENT_TYPING), enabled && typing ? ANIM_REQUEST_START : ANIM_REQUEST_STOP);
	}, std::placeholders::_2));
	gSavedSettings.getControl("AOSitsEnabled")->getSignal()->connect(boost::bind(&LLFloaterAO::run));
	sSwimming = is_underwater();
	gSavedSettings.getControl("AOSwimEnabled")->getSignal()->connect(boost::bind(&LLFloaterAO::toggleSwim, boost::bind(is_underwater)));
}

void LLFloaterAO::onOpen()
{
	LLFirstUse::useAO();
}

LLFloaterAO::~LLFloaterAO()
{
	mcomboBox_stands = nullptr;
	mcomboBox_walks = nullptr;
	mcomboBox_runs = nullptr;
	mcomboBox_jumps = nullptr;
	mcomboBox_sits = nullptr;
	mcomboBox_gsits = nullptr;
	mcomboBox_crouchs = nullptr;
	mcomboBox_cwalks = nullptr;
	mcomboBox_falls = nullptr;
	mcomboBox_hovers = nullptr;
	mcomboBox_flys = nullptr;
	mcomboBox_flyslows = nullptr;
	mcomboBox_flyups = nullptr;
	mcomboBox_flydowns = nullptr;
	mcomboBox_lands = nullptr;
	mcomboBox_typings = nullptr;
	mcomboBox_floats = nullptr;
	mcomboBox_swims = nullptr;
	mcomboBox_swimups = nullptr;
	mcomboBox_swimdowns = nullptr;
	mcomboBox_standups = nullptr;
	mcomboBox_prejumps = nullptr;
//	LL_INFOS() << "floater destroyed" << LL_ENDL;
}

BOOL LLFloaterAO::postBuild()
{
	gSavedSettings.getControl("AOAdvanced")->getSignal()->connect(boost::bind(&LLFloaterAO::updateLayout, this, _2));

	getChild<LLUICtrl>("reloadcard")->setCommitCallback(boost::bind(&LLFloaterAO::onClickReloadCard));
	getChild<LLUICtrl>("opencard")->setCommitCallback(boost::bind(&LLFloaterAO::onClickOpenCard));
	getChild<LLUICtrl>("newcard")->setCommitCallback(boost::bind(&LLFloaterAO::onClickNewCard));
	getChild<LLUICtrl>("prevstand")->setCommitCallback(boost::bind(&LLFloaterAO::onClickCycleStand, false));
	getChild<LLUICtrl>("nextstand")->setCommitCallback(boost::bind(&LLFloaterAO::onClickCycleStand, true));
	getChild<LLUICtrl>("standtime")->setCommitCallback(boost::bind(&LLFloaterAO::onSpinnerCommit));

	(mcomboBox_stands = getChild<LLComboBox>("stands"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit,_1));
	(mcomboBox_walks = getChild<LLComboBox>("walks"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit,_1));
	(mcomboBox_runs = getChild<LLComboBox>("runs"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit,_1));
	(mcomboBox_jumps = getChild<LLComboBox>("jumps"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit,_1));
	(mcomboBox_sits = getChild<LLComboBox>("sits"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit,_1));
	(mcomboBox_gsits = getChild<LLComboBox>("gsits"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit,_1));
	(mcomboBox_crouchs = getChild<LLComboBox>("crouchs"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit,_1));
	(mcomboBox_cwalks = getChild<LLComboBox>("cwalks"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit,_1));
	(mcomboBox_falls = getChild<LLComboBox>("falls"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_hovers = getChild<LLComboBox>("hovers"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_flys = getChild<LLComboBox>("flys"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_flyslows = getChild<LLComboBox>("flyslows"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_flyups = getChild<LLComboBox>("flyups"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_flydowns = getChild<LLComboBox>("flydowns"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_lands = getChild<LLComboBox>("lands"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_typings = getChild<LLComboBox>("typings"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_floats = getChild<LLComboBox>("floats"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_swims = getChild<LLComboBox>("swims"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_swimups = getChild<LLComboBox>("swimups"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_swimdowns = getChild<LLComboBox>("swimdowns"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit, _1));
	(mcomboBox_standups = getChild<LLComboBox>("standups"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit,_1));
	(mcomboBox_prejumps = getChild<LLComboBox>("prejumps"))->setCommitCallback(boost::bind(&LLFloaterAO::onComboBoxCommit,_1));

	updateLayout(gSavedSettings.getBOOL("AOAdvanced"));

	return true;
}

void LLFloaterAO::onSpinnerCommit()
{
	if (mAOStandTimer) mAOStandTimer->reset();
}

void LLFloaterAO::onComboBoxCommit(LLUICtrl* ctrl)
{
	if (LLComboBox* box = (LLComboBox*)ctrl)
	{
		if (box->getName() == "stands")
		{
			stand_iterator = box->getCurrentIndex();
			cmdline_printchat(llformat("Changing stand to %s.",mAOStands[stand_iterator].anim_name.c_str()));
			ChangeStand();
		}
		else
		{
			AOState state = getStateFromCombo(box);
			std::string stranim = box->getValue().asString();
//			LL_INFOS() << "state " << state << " - " << getAnimationState() << LL_ENDL;
			gAgent.sendAnimationRequest(GetAnimIDFromState(state), ANIM_REQUEST_STOP);
			bool start_cond = getAnimationState() == state && gSavedSettings.getBOOL("AOEnabled");
			switch (state)
			{
			case STATE_AGENT_SIT:
			case STATE_AGENT_GROUNDSIT:
				start_cond &= gAgentAvatarp && gAgentAvatarp->isSitting() && gSavedSettings.getBOOL("AOSitsEnabled");
				break;
			default: break;
			}
			LLUUID anim = getAssetIDByName(stranim);
			if (start_cond) gAgent.sendAnimationRequest(anim, ANIM_REQUEST_START);
			gSavedPerAccountSettings.setString(ctrl->getControlName(), stranim);
			for (auto& ao : mAOOverrides)
			{
				if (state == ao.state)
				{
					ao.ao_id = anim;
					break;
				}
			}
		}
	}
}

void LLFloaterAO::updateLayout(bool advanced)
{
	reshape(advanced ? 800 : 200, getRect().getHeight());
	childSetVisible("tabcontainer", advanced);
}

void LLFloaterAO::init()
{
	mAOStands.clear();
	mAOOverrides.clear();

	struct_overrides overrideloader;
	overrideloader.orig_id = ANIM_AGENT_WALK;					overrideloader.state = STATE_AGENT_WALK;			mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_RUN;					overrideloader.state = STATE_AGENT_RUN;			mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_PRE_JUMP;				overrideloader.state = STATE_AGENT_PRE_JUMP;		mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_JUMP;					overrideloader.state = STATE_AGENT_JUMP;			mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_TURNLEFT;				overrideloader.state = STATE_AGENT_TURNLEFT;		mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_TURNRIGHT;				overrideloader.state = STATE_AGENT_TURNRIGHT;		mAOOverrides.push_back(overrideloader);

	overrideloader.orig_id = ANIM_AGENT_SIT;					overrideloader.state = STATE_AGENT_SIT;			mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_SIT_FEMALE;				overrideloader.state = STATE_AGENT_SIT;			mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_SIT_GENERIC;			overrideloader.state = STATE_AGENT_SIT;			mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_SIT_GROUND;				overrideloader.state = STATE_AGENT_GROUNDSIT;		mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_SIT_GROUND_CONSTRAINED;	overrideloader.state = STATE_AGENT_GROUNDSIT;		mAOOverrides.push_back(overrideloader);

	overrideloader.orig_id = ANIM_AGENT_HOVER;					overrideloader.state = STATE_AGENT_HOVER;			mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_HOVER_DOWN;				overrideloader.state = STATE_AGENT_HOVER_DOWN;		mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_HOVER_UP;				overrideloader.state = STATE_AGENT_HOVER_UP;		mAOOverrides.push_back(overrideloader);

	overrideloader.orig_id = ANIM_AGENT_CROUCH;					overrideloader.state = STATE_AGENT_CROUCH;			mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_CROUCHWALK;				overrideloader.state = STATE_AGENT_CROUCHWALK;		mAOOverrides.push_back(overrideloader);

	overrideloader.orig_id = ANIM_AGENT_FALLDOWN;				overrideloader.state = STATE_AGENT_FALLDOWN;		mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_STANDUP;				overrideloader.state = STATE_AGENT_STANDUP;		mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_LAND;					overrideloader.state = STATE_AGENT_LAND;			mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_TYPE;					overrideloader.state = STATE_AGENT_TYPING;			mAOOverrides.push_back(overrideloader);

	overrideloader.orig_id = ANIM_AGENT_FLY;					overrideloader.state = STATE_AGENT_FLY;			mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_FLYSLOW;				overrideloader.state = STATE_AGENT_FLYSLOW;		mAOOverrides.push_back(overrideloader);

	overrideloader.orig_id = ANIM_AGENT_HOVER;					overrideloader.state = STATE_AGENT_FLOAT;			mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_HOVER_DOWN;				overrideloader.state = STATE_AGENT_SWIM_DOWN;		mAOOverrides.push_back(overrideloader);
	overrideloader.orig_id = ANIM_AGENT_HOVER_UP;				overrideloader.state = STATE_AGENT_SWIM_UP;		mAOOverrides.push_back(overrideloader);

	overrideloader.orig_id = ANIM_AGENT_FLY;					overrideloader.state = STATE_AGENT_SWIM;			mAOOverrides.push_back(overrideloader);

	bool success = true;

	if(LLStartUp::getStartupState() >= STATE_INVENTORY_SEND)
	{
		if(LLInventoryModelBackgroundFetch::instance().isEverythingFetched())
		{
			LLUUID configncitem = (LLUUID)gSavedPerAccountSettings.getString("AOConfigNotecardID");
			if (configncitem.notNull())
			{
				success = false;
				const LLInventoryItem* item = gInventory.getItem(configncitem);
				if(item)
				{
					if (gAgent.allowOperation(PERM_COPY, item->getPermissions(),GP_OBJECT_MANIPULATE) || gAgent.isGodlike())
					{
						if(!item->getAssetUUID().isNull())
						{
							LLUUID* new_uuid = new LLUUID(configncitem);
							LLHost source_sim = LLHost::invalid;
							invfolderid = item->getParentUUID();
							gAssetStorage->getInvItemAsset(source_sim,
															gAgent.getID(),
															gAgent.getSessionID(),
															item->getPermissions().getOwner(),
															LLUUID::null,
															item->getUUID(),
															item->getAssetUUID(),
															item->getType(),
															&onNotecardLoadComplete,
															(void*)new_uuid,
															true);
							success = true;
						}
					}
				}
			}
		}
	}

	if (!success)
	{
		cmdline_printchat("Could not read the specified Config Notecard");
	}

//	mAnimationState = STATE_AGENT_IDLE;
//	mCurrentStandId = LLUUID::null;
//	setAnimationState(STATE_AGENT_IDLE);
}


void LLFloaterAO::run()
{
	setAnimationState(STATE_AGENT_IDLE); // reset state
	AOState state = getAnimationState(); // check if sitting or hovering
	bool enabled = gSavedSettings.getBOOL("AOEnabled");
	if (state == STATE_AGENT_IDLE || state == STATE_AGENT_STAND)
	{
		if (enabled)
		{
			if (mAOStandTimer)
			{
				mAOStandTimer->reset();
				ChangeStand();
			}
			else
			{
				mAOStandTimer =	new AOStandTimer();
			}
		}
		else
		{
			stopMotion(getCurrentStandId(), true); //stop stand first then set state
			setAnimationState(STATE_AGENT_IDLE);
		}
	}
	else
	{
		bool sit = false;
		switch (state)
		{
		case STATE_AGENT_SIT:
		case STATE_AGENT_GROUNDSIT:
			sit = true;
			stopMotion(getCurrentStandId(), true); // Stop the current stand
			break;
		default: break;
		}
		gAgent.sendAnimationRequest(GetAnimIDFromState(state), (enabled &&  (!sit || gSavedSettings.getBOOL("AOSitsEnabled"))) ? ANIM_REQUEST_START : ANIM_REQUEST_STOP);
	}
}

void LLFloaterAO::typing(bool start)
{
	uuid_vec_t anims;
	// If we're stopping, stop regardless, just in case the setting was toggled during (e.g.: keyboard shortcut)
	if (!start || gSavedSettings.getBOOL("PlayTypingAnim")) // Linden typing
		anims.push_back(ANIM_AGENT_TYPE);
	if (gSavedSettings.getBOOL("AOEnabled")) // Typing override
		anims.push_back(GetAnimIDFromState(STATE_AGENT_TYPING));
	if (anims.empty()) return;
	gAgent.sendAnimationRequests(anims, start ? ANIM_REQUEST_START : ANIM_REQUEST_STOP);
}

AOState LLFloaterAO::flyToSwimState(const AOState& state)
{
	switch (state)
	{
	case STATE_AGENT_HOVER: return STATE_AGENT_FLOAT;
	case STATE_AGENT_FLY: return STATE_AGENT_SWIM;
	case STATE_AGENT_HOVER_UP: return STATE_AGENT_SWIM_UP;
	case STATE_AGENT_HOVER_DOWN: return STATE_AGENT_SWIM_DOWN;
	default: return state;
	}
}

AOState LLFloaterAO::swimToFlyState(const AOState& state)
{
	switch (state)
	{
	case STATE_AGENT_FLOAT: return STATE_AGENT_HOVER;
	case STATE_AGENT_SWIM: return STATE_AGENT_FLY;
	case STATE_AGENT_SWIM_UP: return STATE_AGENT_HOVER_UP;
	case STATE_AGENT_SWIM_DOWN: return STATE_AGENT_HOVER_DOWN;
	default: return state;
	}
}

AOState LLFloaterAO::getAnimationState()
{
	if (gAgentAvatarp)
	{
		if (gAgentAvatarp->isSitting()) setAnimationState(STATE_AGENT_SIT);
		else if (gAgent.getFlying()) setAnimationState(STATE_AGENT_HOVER);
	}
	return mAnimationState;
}

void LLFloaterAO::setAnimationState(const AOState& state)
{
	mAnimationState = sSwimming ? flyToSwimState(state) : state;
}

LLUUID LLFloaterAO::getCurrentStandId()
{
	return mCurrentStandId;
}

void LLFloaterAO::setCurrentStandId(const LLUUID& id)
{
	mCurrentStandId = id;
}

bool LLFloaterAO::swimCheck(const AOState& state)
{
	switch (state)
	{
		case STATE_AGENT_HOVER:
		case STATE_AGENT_FLY:
		case STATE_AGENT_HOVER_UP:
		case STATE_AGENT_HOVER_DOWN:
			return !sSwimming;
		case STATE_AGENT_FLOAT:
		case STATE_AGENT_SWIM:
		case STATE_AGENT_SWIM_UP:
		case STATE_AGENT_SWIM_DOWN:
			return sSwimming;
		default: return true;
	}
}

LLUUID LLFloaterAO::GetAnimID(const LLUUID& id)
{
	for (const auto& ao : mAOOverrides)
	{
		if (swimCheck(ao.state) && ao.orig_id == id)
		{
			return ao.ao_id;
		}
	}
	return LLUUID::null;
}

AOState LLFloaterAO::GetStateFromAnimID(const LLUUID& id)
{
	for (const auto& ao : mAOOverrides)
	{
		if (swimCheck(ao.state) && ao.orig_id == id) return ao.state;
	}
	return STATE_AGENT_IDLE;
}

LLUUID LLFloaterAO::GetAnimIDFromState(const AOState& state)
{
	for (const auto& ao : mAOOverrides)
	{
		if (ao.state == state) return ao.ao_id;
	}
	return LLUUID::null;
}

AOState LLFloaterAO::GetStateFromToken(std::string strtoken)
{
	if (strtoken == "[ Sitting On Ground ]") return STATE_AGENT_GROUNDSIT;
	if (strtoken == "[ Sitting ]") return STATE_AGENT_SIT;
	if (strtoken == "[ Crouching ]") return STATE_AGENT_CROUCH;
	if (strtoken == "[ Crouch Walking ]") return STATE_AGENT_CROUCHWALK;
	if (strtoken == "[ Standing Up ]") return STATE_AGENT_STANDUP;
	if (strtoken == "[ Falling ]") return STATE_AGENT_FALLDOWN;
	if (strtoken == "[ Flying Down ]") return STATE_AGENT_HOVER_DOWN;
	if (strtoken == "[ Flying Up ]") return STATE_AGENT_HOVER_UP;
	if (strtoken == "[ Flying Slow ]") return STATE_AGENT_FLYSLOW;
	if (strtoken == "[ Flying ]") return STATE_AGENT_FLY;
	if (strtoken == "[ Hovering ]") return STATE_AGENT_HOVER;
	if (strtoken == "[ Jumping ]") return STATE_AGENT_JUMP;
	if (strtoken == "[ Pre Jumping ]") return STATE_AGENT_PRE_JUMP;
	if (strtoken == "[ Running ]") return STATE_AGENT_RUN;
	if (strtoken == "[ Turning Right ]") return STATE_AGENT_TURNRIGHT;
	if (strtoken == "[ Turning Left ]") return STATE_AGENT_TURNLEFT;
	if (strtoken == "[ Walking ]") return STATE_AGENT_WALK;
	if (strtoken == "[ Landing ]") return STATE_AGENT_LAND;
	if (strtoken == "[ Standing ]") return STATE_AGENT_STAND;
	if (strtoken == "[ Swimming Down ]") return STATE_AGENT_SWIM_DOWN;
	if (strtoken == "[ Swimming Up ]") return STATE_AGENT_SWIM_UP;
	if (strtoken == "[ Swimming Forward ]") return STATE_AGENT_SWIM;
	if (strtoken == "[ Floating ]") return STATE_AGENT_FLOAT;
	if (strtoken == "[ Typing ]") return STATE_AGENT_TYPING;
	return STATE_AGENT_IDLE;
}

void LLFloaterAO::onClickCycleStand(bool next)
{
	if (mAOStands.empty()) return;
	stand_iterator += (next ? 1 : -1);
	if (stand_iterator < 0) stand_iterator = int( mAOStands.size()-stand_iterator);
	if (stand_iterator > int( mAOStands.size()-1)) stand_iterator = 0;
	cmdline_printchat(llformat("Changing stand to %s.",mAOStands[stand_iterator].anim_name.c_str()));
	ChangeStand();
}

void LLFloaterAO::ChangeStand()
{
	if (gSavedSettings.getBOOL("AOEnabled"))
	{
		if (gAgentAvatarp)
		{
			if (gSavedSettings.getBOOL("AONoStandsInMouselook") && gAgentCamera.cameraMouselook()) return;

			if (gAgentAvatarp->isSitting())
			{
//				stopMotion(getCurrentStandId(), true); //stop stand first then set state
//				if (getAnimationState() != STATE_AGENT_GROUNDSIT) setAnimationState(STATE_AGENT_SIT);
//				setCurrentStandId(LLUUID::null);
				return;
			}
		}
		AOState state = getAnimationState();
		if (state == STATE_AGENT_IDLE || state == STATE_AGENT_STAND)// stands have lowest priority
		{
			if (mAOStands.empty()) return;
			if (gSavedSettings.getBOOL("AOStandRandomize"))
			{
				stand_iterator = ll_rand(mAOStands.size()-1);
			}
			if (stand_iterator < 0) stand_iterator = int( mAOStands.size()-stand_iterator);
			if (stand_iterator > int( mAOStands.size()-1)) stand_iterator = 0;

			int stand_iterator_previous = stand_iterator -1;

			if (stand_iterator_previous < 0) stand_iterator_previous = int( mAOStands.size()-1);
			
			if (mAOStands[stand_iterator].ao_id.notNull())
			{
				stopMotion(getCurrentStandId(), true); //stop stand first then set state
				startMotion(mAOStands[stand_iterator].ao_id, true);

				setAnimationState(STATE_AGENT_STAND);
				setCurrentStandId(mAOStands[stand_iterator].ao_id);
				if (mcomboBox_stands) mcomboBox_stands->selectNthItem(stand_iterator);
//				LL_INFOS() << "changing stand to " << mAOStands[stand_iterator].anim_name << LL_ENDL;
			}
		}
	} 
	else stopMotion(getCurrentStandId(), true);
}

void LLFloaterAO::toggleSwim(bool underwater)
{
	static const LLCachedControl<bool> enabled(gSavedSettings, "AOEnabled", false);

	sSwimming = underwater && enable_swim();

	// Don't send requests if we have the AO disabled.
	if (enabled)
	{
		AOState state = getAnimationState();
		if (state != STATE_AGENT_IDLE && state != STATE_AGENT_STAND) // Don't bother if we're just standing or idle (Who pushed us?!)
		{
			// Stop all of the previous states
			constexpr std::array<AOState, 4> swim_states = { STATE_AGENT_FLOAT, STATE_AGENT_SWIM, STATE_AGENT_SWIM_UP, STATE_AGENT_SWIM_DOWN };
			constexpr std::array<AOState, 4> fly_states = { STATE_AGENT_HOVER, STATE_AGENT_FLY, STATE_AGENT_HOVER_UP, STATE_AGENT_HOVER_DOWN };
			uuid_vec_t vec;
			for (const auto& state : sSwimming ? fly_states : swim_states)
				vec.push_back(GetAnimIDFromState(state));
			gAgent.sendAnimationRequests(vec, ANIM_REQUEST_STOP);
			// Process new animations
			gAgentAvatarp->processAnimationStateChanges();
		}
	}
}

void LLFloaterAO::doMotion(const LLUUID& id, bool start, bool stand)
{
	if (id.isNull()) return;
	LLUUID anim = id;
	if (stand)
	{
		if (start)
		{
			if (gAgentAvatarp && gAgentAvatarp->isSitting())
				return;
		}
		else setAnimationState(STATE_AGENT_IDLE);
	}
	else
	{
		anim = GetAnimID(id);
		if (anim.isNull() || !gSavedSettings.getBOOL("AOEnabled")) return;
		AOState state = GetStateFromAnimID(id);
		if (start)
		{
			stopMotion(getCurrentStandId(), true); //stop stand first then set state 
			setAnimationState(state);
//			LL_INFOS() << " state " << getAnimationState() << " start anim " << id << " overriding with " << anim << LL_ENDL;
			if (state == STATE_AGENT_SIT && !gSavedSettings.getBOOL("AOSitsEnabled")) return;
		}
		else
		{
//			LL_INFOS() << " state " << getAnimationState() << "/" << state << "(now 0)  stop anim " << id << " overriding with " << anim << LL_ENDL;
			if (getAnimationState() == state) setAnimationState(STATE_AGENT_IDLE);
			ChangeStand(); // startMotion(getCurrentStandId(), true);
		}
	}
	gAgent.sendAnimationRequest(anim, start ? ANIM_REQUEST_START : ANIM_REQUEST_STOP);
}

void LLFloaterAO::onClickReloadCard()
{
	if (LLInventoryModelBackgroundFetch::instance().isEverythingFetched())
	{
		init();
	}
}

void LLFloaterAO::onClickOpenCard()
{
	if (LLInventoryModelBackgroundFetch::instance().isEverythingFetched())
	{
		LLUUID configncitem = (LLUUID)gSavedPerAccountSettings.getString("AOConfigNotecardID");
		if (configncitem.notNull())
		{
			if (LLViewerInventoryItem* item = gInventory.getItem(configncitem))
			{
				if (gAgent.allowOperation(PERM_COPY, item->getPermissions(),GP_OBJECT_MANIPULATE) || gAgent.isGodlike())
				{
					if(!item->getAssetUUID().isNull())
						open_notecard(item, "Note: " + item->getName(), LLUUID::null, false);
				}
			}
		}
	}
}

void LLFloaterAO::onClickNewCard()
{
	// load the template file from app_settings/ao_template.ini then
	// create a new properly-formatted notecard in the user's inventory
	std::string ao_template = gDirUtilp->getExpandedFilename(LL_PATH_APP_SETTINGS, "ao_template.ini");
	if (!ao_template.empty())
	{
		LLPointer<LLInventoryCallback> cb = new AONotecardCallback(ao_template);
		create_inventory_item(gAgentID, gAgentSessionID,
							LLUUID::null, LLTransactionID::tnull, "New AO Notecard", 
							"Drop this notecard in your AO window to use", LLAssetType::AT_NOTECARD,
							LLInventoryType::IT_NOTECARD, NOT_WEARABLE, PERM_ALL, cb);
	}
	else
	{
		LL_WARNS() << "Can't find ao_template.ini in app_settings!" << LL_ENDL;
	}	
}

struct AOAssetInfo
{
	std::string path, name;
};

AOState LLFloaterAO::getStateFromCombo(const LLComboBox* combo)
{
	if (combo == mcomboBox_stands) return STATE_AGENT_STAND;
	if (combo == mcomboBox_walks) return STATE_AGENT_WALK;
	if (combo == mcomboBox_runs) return STATE_AGENT_RUN;
	if (combo == mcomboBox_jumps) return STATE_AGENT_JUMP;
	if (combo == mcomboBox_sits) return STATE_AGENT_SIT;
	if (combo == mcomboBox_gsits) return STATE_AGENT_GROUNDSIT;
	if (combo == mcomboBox_crouchs) return STATE_AGENT_CROUCH;
	if (combo == mcomboBox_cwalks) return STATE_AGENT_CROUCHWALK;
	if (combo == mcomboBox_falls) return STATE_AGENT_FALLDOWN;
	if (combo == mcomboBox_hovers) return STATE_AGENT_HOVER;
	if (combo == mcomboBox_flys) return STATE_AGENT_FLY;
	if (combo == mcomboBox_flyslows) return STATE_AGENT_FLYSLOW;
	if (combo == mcomboBox_flyups) return STATE_AGENT_HOVER_UP;
	if (combo == mcomboBox_flydowns) return STATE_AGENT_HOVER_DOWN;
	if (combo == mcomboBox_lands) return STATE_AGENT_LAND;
	if (combo == mcomboBox_typings) return STATE_AGENT_TYPING;
	if (combo == mcomboBox_floats) return STATE_AGENT_FLOAT;
	if (combo == mcomboBox_swims) return STATE_AGENT_SWIM;
	if (combo == mcomboBox_swimups) return STATE_AGENT_SWIM_UP;
	if (combo == mcomboBox_swimdowns) return STATE_AGENT_SWIM_DOWN;
	if (combo == mcomboBox_standups) return STATE_AGENT_STANDUP;
	if (combo == mcomboBox_prejumps) return STATE_AGENT_PRE_JUMP;
	return STATE_AGENT_IDLE;
}

LLComboBox* LLFloaterAO::getComboFromState(const AOState& state)
{
	switch (state)
	{
	case STATE_AGENT_STAND: return mcomboBox_stands;
	case STATE_AGENT_WALK: return mcomboBox_walks;
	case STATE_AGENT_RUN: return mcomboBox_runs;
	case STATE_AGENT_JUMP: return mcomboBox_jumps;
	case STATE_AGENT_SIT: return mcomboBox_sits;
	case STATE_AGENT_GROUNDSIT: return mcomboBox_gsits;
	case STATE_AGENT_CROUCH: return mcomboBox_crouchs;
	case STATE_AGENT_CROUCHWALK: return mcomboBox_cwalks;
	case STATE_AGENT_FALLDOWN: return mcomboBox_falls;
	case STATE_AGENT_HOVER: return mcomboBox_hovers;
	case STATE_AGENT_FLY: return mcomboBox_flys;
	case STATE_AGENT_FLYSLOW: return mcomboBox_flyslows;
	case STATE_AGENT_HOVER_UP: return mcomboBox_flyups;
	case STATE_AGENT_HOVER_DOWN: return mcomboBox_flydowns;
	case STATE_AGENT_LAND: return mcomboBox_lands;
	case STATE_AGENT_TYPING: return mcomboBox_typings;
	case STATE_AGENT_FLOAT: return mcomboBox_floats;
	case STATE_AGENT_SWIM: return mcomboBox_swims;
	case STATE_AGENT_SWIM_UP: return mcomboBox_swimups;
	case STATE_AGENT_SWIM_DOWN: return mcomboBox_swimdowns;
	case STATE_AGENT_STANDUP: return mcomboBox_standups;
	case STATE_AGENT_PRE_JUMP: return mcomboBox_prejumps;
	default: return nullptr;
	}
}

void LLFloaterAO::onNotecardLoadComplete(LLVFS *vfs,const LLUUID& asset_uuid,LLAssetType::EType type,void* user_data, S32 status, LLExtStat ext_status)
{
	if(status == LL_ERR_NOERR)
	{
		if(type == LLAssetType::AT_NOTECARD)
		{
			LLVFile file(vfs, asset_uuid, type, LLVFile::READ);
			S32 size = file.getSize();

			char* buffer = new char[size];
			if (buffer == NULL)
			{
				LL_ERRS() << "Memory Allocation Failed" << LL_ENDL;
				return;
			}

			file.read((U8*)buffer, size);

			LLViewerTextEditor* edit = new LLViewerTextEditor("",LLRect(0,0,0,0),S32_MAX,"");
			if(edit->importBuffer(buffer, (S32)size))
			{
				LL_INFOS() << "ao nc decode success" << LL_ENDL;
				std::string card = edit->getText();
				edit->die();

				if (mcomboBox_stands)
				{
					mcomboBox_stands->clear();
					mcomboBox_stands->removeall();
				}
				if (mcomboBox_walks) mcomboBox_walks->clear();
				if (mcomboBox_runs) mcomboBox_runs->clear();
				if (mcomboBox_jumps) mcomboBox_jumps->clear();
				if (mcomboBox_sits) mcomboBox_sits->clear();
				if (mcomboBox_gsits) mcomboBox_gsits->clear();
				if (mcomboBox_crouchs) mcomboBox_cwalks->clear();
				if (mcomboBox_cwalks) mcomboBox_cwalks->clear();
				if (mcomboBox_falls) mcomboBox_falls->clear();
				if (mcomboBox_hovers) mcomboBox_hovers->clear();
				if (mcomboBox_flys) mcomboBox_flys->clear();
				if (mcomboBox_flyslows) mcomboBox_flyslows->clear();
				if (mcomboBox_flyups) mcomboBox_flyups->clear();
				if (mcomboBox_flydowns) mcomboBox_flydowns->clear();
				if (mcomboBox_lands) mcomboBox_lands->clear();
				if (mcomboBox_typings) mcomboBox_typings->clear();
				if (mcomboBox_floats) mcomboBox_floats->clear();
				if (mcomboBox_swims) mcomboBox_swims->clear();
				if (mcomboBox_swimups) mcomboBox_swimups->clear();
				if (mcomboBox_swimdowns) mcomboBox_swimdowns->clear();
				if (mcomboBox_standups) mcomboBox_standups->clear();
				if (mcomboBox_prejumps) mcomboBox_prejumps->clear();

				typedef boost::tokenizer<boost::char_separator<char> > tokenizer;
				boost::char_separator<char> sep("\n");
				tokenizer tokline(card, sep);

				for (const auto& strline : tokline)
				{
//					LL_INFOS() << "uncommented line: " << strline << LL_ENDL;

					boost::regex type("^(\\s*)(\\[ )(.*)( \\])");
					boost::smatch what; 
					if (boost::regex_search(strline, what, type)) 
					{
						std::string strtoken(what[0]);
//						LL_INFOS() << "type: " << strtoken << LL_ENDL;
//						LL_INFOS() << "anims in type: " << boost::regex_replace(strline, type, "") << LL_ENDL;

						boost::char_separator<char> sep("|,");
						std::string stranimnames(boost::regex_replace(strline, type, ""));
						tokenizer tokanimnames(stranimnames, sep);
						for (const auto& stranim : tokanimnames)
						{
							LLUUID animid(getAssetIDByName(stranim));

//							LL_INFOS() << invfolderid.asString().c_str() << LL_ENDL;
//							LL_INFOS() << "anim: " << stranim.c_str() << " assetid: " << animid << LL_ENDL;
							if (animid.isNull())
							{
								cmdline_printchat(llformat("Warning: animation '%s' could not be found (Section: %s).",stranim.c_str(),strtoken.c_str()));
							}
							else
							{
								const AOState state = GetStateFromToken(strtoken);
								switch (state)
								{
								case STATE_AGENT_STAND:
									mAOStands.push_back({ animid, stranim });
								default: break;
								}

								if (LLComboBox* combo = getComboFromState(state))
								{
									//LL_INFOS() << "1 anim: " << stranim.c_str() << " assetid: " << animid << LL_ENDL;
									if (!combo->selectByValue(stranim)) //check if exist
										combo->add(stranim, ADD_BOTTOM, true);
								}

								for (auto& ao : mAOOverrides)
								{
									if (state == ao.state)
									{
										ao.ao_id = animid;
										break;
									}
								}
							}
						}
					} 
				}
				LL_INFOS() << "ao nc read sucess" << LL_ENDL;

				for (auto& ao : mAOOverrides)
				{
					if (LLComboBox* combo = getComboFromState(ao.state))
					{
						std::string defaultanim = gSavedPerAccountSettings.getString(combo->getControlName());
						const LLUUID ao_id = getAssetIDByName(defaultanim);
						if (ao_id != LLUUID::null) ao.ao_id = ao_id;
						combo->selectByValue(defaultanim);
					}
				}
				run();
			}
			else
			{
				LL_INFOS() << "ao nc decode error" << LL_ENDL;
			}
			delete[] buffer;
		}
	}
	else
	{
		LL_INFOS() << "ao nc read error" << LL_ENDL;
	}
}

class ObjectNameMatches : public LLInventoryCollectFunctor
{
public:
	ObjectNameMatches(std::string name) : sName(name) {}
	virtual ~ObjectNameMatches() {}
	virtual bool operator()(LLInventoryCategory* cat, LLInventoryItem* item)
	{
		return item && item->getParentUUID() == LLFloaterAO::invfolderid && item->getName() == sName;
	}
private:
	std::string sName;
};

const LLUUID& LLFloaterAO::getAssetIDByName(const std::string& name)
{
	if (name.empty() || !LLInventoryModelBackgroundFetch::instance().isEverythingFetched()) return LLUUID::null;

	LLViewerInventoryCategory::cat_array_t cats;
	LLViewerInventoryItem::item_array_t items;
	ObjectNameMatches matches(name);
	gInventory.collectDescendentsIf(LLUUID::null,cats,items,false, matches);

	return items.empty() ? LLUUID::null : items[0]->getAssetUUID();
};
