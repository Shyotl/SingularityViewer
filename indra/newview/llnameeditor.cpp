/** 
 * @file llnameeditor.cpp
 * @brief Name Editor to refresh a name.
 *
 * $LicenseInfo:firstyear=2003&license=viewergpl$
 * 
 * Copyright (c) 2003-2009, Linden Research, Inc.
 * 
 * Second Life Viewer Source Code
 * The source code in this file ("Source Code") is provided by Linden Lab
 * to you under the terms of the GNU General Public License, version 2.0
 * ("GPL"), unless you have obtained a separate licensing agreement
 * ("Other License"), formally executed by you and Linden Lab.  Terms of
 * the GPL can be found in doc/GPL-license.txt in this distribution, or
 * online at http://secondlifegrid.net/programs/open_source/licensing/gplv2
 * 
 * There are special exceptions to the terms and conditions of the GPL as
 * it is applied to this Source Code. View the full text of the exception
 * in the file doc/FLOSS-exception.txt in this software distribution, or
 * online at
 * http://secondlifegrid.net/programs/open_source/licensing/flossexception
 * 
 * By copying, modifying or distributing this software, you acknowledge
 * that you have read and understood your obligations described above,
 * and agree to abide by those obligations.
 * 
 * ALL LINDEN LAB SOURCE CODE IS PROVIDED "AS IS." LINDEN LAB MAKES NO
 * WARRANTIES, EXPRESS, IMPLIED OR OTHERWISE, REGARDING ITS ACCURACY,
 * COMPLETENESS OR PERFORMANCE.
 * $/LicenseInfo$
 */

#include "llviewerprecompiledheaders.h"
 
#include "llnameeditor.h"
#include "llcachename.h"
#include "llmenugl.h"
#include "lluictrlfactory.h"

static LLRegisterWidget<LLNameEditor> r("name_editor");

LLNameEditor::LLNameEditor(const std::string& name, const LLRect& rect,
		const LLUUID& name_id,
		bool is_group,
		const std::string& loading,
		const LLFontGL* glfont,
		S32 max_text_length)
: LLNameUI(loading, name_id, is_group)
, LLLineEditor(name, rect, LLStringUtil::null, glfont, max_text_length)
{
	if (!name_id.isNull())
	{
		setNameID(name_id, is_group);
	}
	else setText(mInitialValue);
}

// virtual
BOOL LLNameEditor::handleRightMouseDown(S32 x, S32 y, MASK mask)
{
	bool simple_menu = mContextMenuHandle.get()->getName() != "rclickmenu";
	std::string new_menu;
	// Singu TODO: Generic menus for groups
	if ((mIsGroup || mNameID.isNull()) && simple_menu)
	{
		new_menu = "menu_texteditor.xml";
	}
	else if (!simple_menu && !mIsGroup)
	{
		new_menu = "menu_nameeditor_avatar.xml";
	}
	if (!new_menu.empty()) setContextMenu(LLUICtrlFactory::instance().buildMenu(new_menu, LLMenuGL::sMenuContainer));

	return LLLineEditor::handleRightMouseDown(x, y, mask);
}

void LLNameEditor::setText(const std::string& text)
{
	LLLineEditor::setText(text);
}

void LLNameEditor::setValue( const LLSD& value )
{
	setNameID(value.asUUID(), FALSE);
}

LLSD LLNameEditor::getValue() const
{
	return LLSD(mNameID);
}

// virtual
LLXMLNodePtr LLNameEditor::getXML(bool save_children) const
{
	LLXMLNodePtr node = LLLineEditor::getXML();

	node->setName(LL_NAME_EDITOR_TAG);

	return node;
}

LLView* LLNameEditor::fromXML(LLXMLNodePtr node, LLView *parent, LLUICtrlFactory *factory)
{
	LLRect rect;
	createRect(node, rect, parent, LLRect());

	S32 max_text_length = 1024;
	node->getAttributeS32("max_length", max_text_length);
	bool is_group = false;
	node->getAttribute_bool("is_group", is_group);
	LLUUID id;
	node->getAttributeUUID("id", id);
	std::string loading;
	node->getAttributeString("label", loading);

	LLNameEditor* line_editor = new LLNameEditor("name_editor",
								rect,
								id, is_group, loading,
								LLView::selectFont(node),
								max_text_length);

	line_editor->setColorParameters(node);
	line_editor->initFromXML(node, parent);
	
	return line_editor;
}
