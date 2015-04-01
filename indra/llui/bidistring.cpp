#include "bidistring.h"

const CBidiString::context_data_t* CBidiString::getContext(const llwchar wchar)
{
	static CBidiString::context_data_t sContexts[] = {
		{ 0x0622, 0xFE81, 0xFE82, 0x0000, 0x0000 },	//ALEF WITH MADDA ABOVE
		{ 0x0623, 0xFE83, 0xFE84, 0x0000, 0x0000 },	//ALEF WITH HAMZA ABOVE
		{ 0x0624, 0xFE85, 0xFE86, 0x0000, 0x0000 },	//WAW WITH HAMZA ABOVE
		{ 0x0625, 0xFE87, 0xFE88, 0x0000, 0x0000 },	//ALEF WITH HAMZA BELOW
		{ 0x0626, 0xFE89, 0xFE8A, 0xFE8B, 0xFE8C },	//YEH WITH HAMZA ABOVE
		{ 0x0627, 0xFE8D, 0xFE8E, 0x0000, 0x0000 },	//ALEF
		{ 0x0628, 0xFE8F, 0xFE90, 0xFE91, 0xFE92 },	//BEH
		{ 0x0629, 0xFE93, 0xFE94, 0x0000, 0x0000 },	//TEH MARBUTA
		{ 0x062A, 0xFE95, 0xFE96, 0xFE97, 0xFE98 },	//TEH
		{ 0x062B, 0xFE99, 0xFE9A, 0xFE9B, 0xFE9C },	//THEH
		{ 0x062C, 0xFE9D, 0xFE9E, 0xFE9F, 0xFEA0 },	//JEEM
		{ 0x062D, 0xFEA1, 0xFEA2, 0xFEA3, 0xFEA4 },	//HAH
		{ 0x062E, 0xFEA5, 0xFEA6, 0xFEA7, 0xFEA8 },	//KHAH
		{ 0x062F, 0xFEA9, 0xFEAA, 0x0000, 0x0000 },	//DAL
		{ 0x0630, 0xFEAB, 0xFEAC, 0x0000, 0x0000 },	//THAL
		{ 0x0631, 0xFEAD, 0xFEAE, 0x0000, 0x0000 },	//REH
		{ 0x0632, 0xFEAF, 0xFEB0, 0x0000, 0x0000 },	//ZAIN
		{ 0x0633, 0xFEB1, 0xFEB2, 0xFEB3, 0xFEB4 },	//SEEN
		{ 0x0634, 0xFEB5, 0xFEB6, 0xFEB7, 0xFEB8 },	//SHEEN
		{ 0x0635, 0xFEB9, 0xFEBA, 0xFEBB, 0xFEBC },	//SAD
		{ 0x0636, 0xFEBD, 0xFEBE, 0xFEBF, 0xFEC0 },	//DAD
		{ 0x0637, 0xFEC1, 0xFEC2, 0xFEC3, 0xFEC4 },	//TAH
		{ 0x0638, 0xFEC5, 0xFEC6, 0xFEC7, 0xFEC8 },	//ZAH
		{ 0x0639, 0xFEC9, 0xFECA, 0xFECB, 0xFECC },	//AIN
		{ 0x063A, 0xFECD, 0xFECE, 0xFECF, 0xFED0 },	//GHAIN
		{ 0x0641, 0xFED1, 0xFED2, 0xFED3, 0xFED4 },	//FEH
		{ 0x0642, 0xFED5, 0xFED6, 0xFED7, 0xFED8 },	//QAF
		{ 0x0643, 0xFED9, 0xFEDA, 0xFEDB, 0xFEDC },	//KAF
		{ 0x0644, 0xFEDD, 0xFEDE, 0xFEDF, 0xFEE0 },	//LAM
		{ 0x0645, 0xFEE1, 0xFEE2, 0xFEE3, 0xFEE4 },	//MEEM
		{ 0x0646, 0xFEE5, 0xFEE6, 0xFEE7, 0xFEE8 },	//NOON
		{ 0x0647, 0xFEE9, 0xFEEA, 0xFEEB, 0xFEEC },	//HEH
		{ 0x0648, 0xFEED, 0xFEEE, 0x0000, 0x0000 },	//WAW
		{ 0x0649, 0xFEEF, 0xFEF0, 0x0000, 0x0000 },	//ALEF MAKSURA
		{ 0x064A, 0xFEF1, 0xFEF2, 0xFEF3, 0xFEF4 },	//YEH
		{ 0x064E, 0xFE76, 0x0000, 0x0000, 0xFE77 },	//FATHA
		{ 0x064F, 0xFE78, 0x0000, 0x0000, 0xFE79 },	//DAMMA
		{ 0x0650, 0xFE7A, 0x0000, 0x0000, 0xFE7B },	//KASRA
		{ 0x0651, 0xFE7C, 0x0000, 0x0000, 0xFE7D },	//SHADDA
		{ 0x0652, 0xFE7E, 0x0000, 0x0000, 0xFE7F },	//SUKUN
	};
	static const U32 sContextsCount = sizeof(sContexts) / sizeof(sContexts[0]);

	if (wchar >= sContexts[0].mGen && wchar <= sContexts[sContextsCount - 1].mGen)
	{
		for (U32 i = 0; i < sContextsCount; ++i)
		{
			if (sContexts[i].mGen == wchar)
				return &sContexts[i];
		}
	}
	return NULL;
}

void CBidiString::generateDisplayString()
{
	static LLWString result;			// String to return if there has been any string manipulation.

	bool direction_determined = false;	// false if RtL_primary hasn't been determined yet.
	bool RtL_primary = false;			// true if is the string is primarily RtL with potentional LtR blocks. false if primarily LtR with potential RtL blocks.

	// An array of RtL sections. Flip the text in these sections if RtL_primary is false, else flip everything NOT in these sections (As we pre-flip the entire string and then fix the LtR text).
	static std::vector < std::pair<string_size_t, string_size_t> > reverse_sections;
	reverse_sections.clear();

	// Start of a span of characters that we might not want to include in RtL if there's LtR right after them.
	LLWString::const_iterator start_IgnoredChars = mInputString.end();
	// Beginning of current RtL section.
	LLWString::const_iterator start_RtL = mInputString.end();
	// Find RtL spans.
	for (LLWString::const_iterator it = mInputString.begin(); it != mInputString.end(); ++it)
	{
		if (*it <= 0x085F && *it >= 0x0590)
		{
			if (!direction_determined)			//  Hit RtL text before LtR. Treat as RtL primary.
			{
				direction_determined = true;
				RtL_primary = true;
			}
			start_IgnoredChars = mInputString.end();	// Clear start_IgnoredChars. RtL section was not broken.
			if (start_RtL == mInputString.end())
			{
				start_RtL = it;
			}
		}
		else if (start_RtL != mInputString.end() && *it == llwchar(' '))
		{
			if (start_IgnoredChars == mInputString.end())
				start_IgnoredChars = it;	// Hit a space just after RtL. Might want to not include it in the RtL section if there's LtR after.
		}
		else if (*it != llwchar(',') && *it != llwchar('.'))	// Commas and periods don't break RtL sections, for now.
		{
			if (!direction_determined)		// Hit LtR text before RtL. Treat as LtR primary.
			{
				direction_determined = true;
				RtL_primary = false;
			}

			if (start_RtL != mInputString.end())
			{
				// Figure out the indices of RtL section, and add them to our reverse_sections array.
				U32 len = (start_IgnoredChars != mInputString.end() ? start_IgnoredChars : it) - start_RtL;
				U32 offs = start_RtL - mInputString.begin();
				reverse_sections.push_back(std::make_pair(offs, offs + len));
				//llinfos << (reverse_sections.size() - 1) << "[" << offs << "," << (offs + len) << ")" << llendl;
				//llinfos << (reverse_sections.size() - 1) << "[" << mDisplayString[offs] << "," << mDisplayString[offs + len] << ")" << llendl;
				start_IgnoredChars = start_RtL = mInputString.end();
			}
		}
	}

	mIsLastCharRtL = start_RtL != mInputString.end();
	// Ended inside an RtL section, so add it to reverse_sections.
	if (mIsLastCharRtL)
	{
		reverse_sections.push_back(std::make_pair(start_RtL - mInputString.begin(), mInputString.length()));
		//llinfos << (reverse_sections.size() - 1) << "[" << reverse_sections.back().first << "," << reverse_sections.back().second << ")" << llendl;
		//llinfos << (reverse_sections.size() - 1) << "[" << mDisplayString[reverse_sections.back().first] << "," << mDisplayString[reverse_sections.back().second] << ")" << llendl;
	}

	// Return our original text if there's nothing to reverse.
	if (reverse_sections.empty())
		return;

	// Copy text to mDisplayString.
	mDisplayString.assign(mInputString.begin(), mInputString.end());
	// Set up the conversion array
	for (array_size_t i = 0; i < mInputString.length(); ++i)
	{
		llassert_always(i < mDisplayToInputArr.capacity());
		mDisplayToInputArr.push_back(std::make_pair(i,false));
	}
	//If RtL primary, reverse the entire string, then un-reverse the LtR bits.
	if (RtL_primary)
	{
		std::reverse(mDisplayString.begin(), mDisplayString.end());
		std::reverse(mDisplayToInputArr.begin(), mDisplayToInputArr.end());
		std::reverse(reverse_sections.begin(), reverse_sections.end());
	}

	U32 last_text_it = 0;
	U32 num = 0;
	std::vector < std::pair<string_size_t, string_size_t> >::iterator it = reverse_sections.begin();
	for (; it != reverse_sections.end(); ++it)
	{
		++num;
		string_size_t begin, end;
		if (!RtL_primary)	//String not pre-reversed. Can use un-altered indices, and reverse the specific RtL sections.
		{
			begin = it->first;
			end = it->second;
			llassert_always(begin < end);
			std::reverse(mDisplayString.begin() + begin, mDisplayString.begin() + end);
			std::reverse(mDisplayToInputArr.begin() + begin, mDisplayToInputArr.begin() + end);
		}
		else				//String pre-reversed. Indices must be adjusted for this. Must also un-reverse text NOT in the RtL section.
		{
			begin = (mDisplayString.length() - it->second);
			end = (mDisplayString.length() - it->first);
			//llinfos << " " << (num - 1) << "[" << begin << "," << end << ")" << llendl;
			//llinfos << " " << (num - 1) << "[" << mDisplayString[begin] << "," << mDisplayString[end] << ")" << llendl;
			llassert_always(begin < end);
			llassert_always(last_text_it <= begin);
			if (last_text_it != begin)
			{
				std::reverse(mDisplayString.begin() + last_text_it, mDisplayString.begin() + begin);
				std::reverse(mDisplayToInputArr.begin() + last_text_it, mDisplayToInputArr.begin() + begin);
			}
			last_text_it = end;
		}

		for (U32 i = begin; i < end; ++i)
			mDisplayToInputArr[i].second = true;

		llassert_always(begin <= mDisplayString.length());
		llassert_always(end <= mDisplayString.length());

		//Each character is contextual to both neighbors, thus keep a stack of three contexts. Each iteration: next becomes cur, cur becomes prev, and a new next is fetched.
		const context_data_t* prev_context = NULL;
		const context_data_t* cur_context = NULL;
		const context_data_t* next_context = getContext(mDisplayString[begin]);	//will become cur immediately on first iteration.
		for (string_size_t it2 = begin; it2 < end; ++it2)
		{
			prev_context = cur_context;
			cur_context = next_context;
			if (it2 + 1 < end)
				next_context = getContext(mDisplayString[it2 + 1]);
			else //Hit the end. No context to fetch.
				next_context = NULL;

			if (cur_context)
			{
				if (!prev_context || !(prev_context->mEnd || prev_context->mMid))
				{
					if (next_context && cur_context->mEnd && (next_context->mBeg || next_context->mMid))
						mDisplayString[it2] = cur_context->mEnd;
					else
						mDisplayString[it2] = cur_context->mIso;
				}
				else
				{
					if (next_context && (next_context->mMid || next_context->mBeg))
						mDisplayString[it2] = cur_context->mMid ? cur_context->mMid : cur_context->mEnd;
					else
						mDisplayString[it2] = cur_context->mBeg ? cur_context->mBeg : cur_context->mIso;
				}
			}
		}
	}
	if (RtL_primary)
	{
		if (last_text_it < mDisplayString.length())	//May have leftover text after the last RtL section that needs to be un-reversed.
		{
			llassert_always(last_text_it < mDisplayString.length());
			std::reverse(mDisplayString.begin() + last_text_it, mDisplayString.end());
			std::reverse(mDisplayToInputArr.begin() + last_text_it, mDisplayToInputArr.end());
		}
	}

	mIsRtL = RtL_primary;

	/*llinfos << "[";
	for (U32 i = 0; i < mDisplayToInputArr.size(); ++i)
	{
		char buff[32] = { 0 };
		wchar_to_utf8chars(mDisplayString[i], buff);
		llcont << "[" << buff << "," << mDisplayToInputArr[i].first << "," << mDisplayToInputArr[i].second << "],";
	}
	llcont << "]" << llendl;*/
}

LLWString CBidiString::substr(string_size_t start, string_size_t count)
{
	update();

	if (!mDisplayToInputArr.empty())
	{
		LLWString ret;
		ret.reserve(mInputString.length() + 1);
		string_size_t end = llmin(mInputString.length(), start + count);

		llassert_always(end <= mDisplayToInputArr.size());
		llassert_always(mDisplayString.size() == mDisplayToInputArr.size());

		for (string_size_t i = start; i < end; ++i)
		{
			llassert_always(mDisplayToInputArr[i].first < mInputString.size());
			ret.push_back(mInputString[mDisplayToInputArr[i].first]);
		}
		return ret;
	}
	else
	{
		llassert_always(start < mInputString.length());
		return mInputString.substr(start, count);
	}
}

const CBidiString::string_size_t CBidiString::erase(string_size_t start, string_size_t count)
{
	update();
	mIsDirty = true;

	if (!mDisplayToInputArr.empty())
	{
		//First index into mInputString where a character is to be removed. LLWString::npos = nothing to remove.
		string_size_t target_start = LLWString::npos;
		string_size_t end = llmin(mInputString.length(), start + count);

		llassert_always(end <= mDisplayToInputArr.size());
		llassert_always(mDisplayString.size() == mDisplayToInputArr.size());

		//Insert null characters for removal in another loop.
		//There are other(safer) ways to do this, but this method requires no new allocations of any sort.
		//This method also minimizes the memory shuffling gymnastics that calling erase numerous times would cause.
		for (array_size_t i = start; i < end; ++i)
		{
			llassert_always(mDisplayToInputArr[i].first < mInputString.size());
			mInputString[mDisplayToInputArr[i].first] = '\0';
			target_start = llmin(string_size_t(mDisplayToInputArr[i].first), target_start);
		}

		if (target_start != LLWString::npos)
		{
			//Iterate down the string and 'skip' null characters by incrementing offs on every match.
			array_size_t offs = 1;
			for (array_size_t i = target_start; i + offs < mInputString.length();)
			{
				if (mInputString[i + offs] == '\0')
				{
					++offs;
					continue;
				}
				mInputString[i] = mInputString[i + offs];
				++i;
			}
			mInputString.resize(mInputString.size() - offs);
		}
	}
	else
	{
		llassert_always(start < mInputString.size());
		mInputString.erase(start, count);
	}
	return start;
}

const CBidiString::string_size_t CBidiString::insert(string_size_t pos, const LLWString& utf32str)
{
	if (utf32str.empty())
		return pos;
	update();
	mIsDirty = true;

	pos = llmin(pos, mInputString.size());
	llwchar c = utf32str[0];

	bool pos_rtl = pos < mDisplayToInputArr.size() && isRtLCharacter(pos);
	bool prev_rtl = pos && pos <= mDisplayToInputArr.size() && isRtLCharacter(pos - 1);
	bool char_always_rtl = (c <= 0x085F && c >= 0x0590); //Truly rtl, always.
	bool char_maybe_rtl = char_always_rtl || ((c == ' ' || c == ',' || c == '.') && pos_rtl); //rtl depending on context or being char_always_rtl.

	U32 insert_pos = pos;
	if (insert_pos < mDisplayToInputArr.size())
		insert_pos = mDisplayToInputArr[pos].first;
	
	if (!mIsRtL)
	{
		if (!mDisplayToInputArr.empty() && !pos && char_always_rtl)
		{
			mInputString.insert(0, utf32str);	//Switching to RtL primary.
			return mInputString.size()+1;
		}

		//Case where inserted character wont get shuffled around.
		else if (pos >= mDisplayToInputArr.size() ||	//No RtL yet or
			!pos_rtl ||									//Next character isn't RtL or
			char_maybe_rtl)								//No directonal change
		{
			if (char_maybe_rtl)
			{
				//If we are inserting RtL at the tail of a string that ends in RtL, then insert at the FRONT of that RtL section.
				if (prev_rtl)
					insert_pos = mDisplayToInputArr[pos - 1].first;
				else//If we inserted RtL then we need to insert AFTER insert_pos, instead of before it.
					insert_pos = llmin(insert_pos + 1, U32(mInputString.size()));
			}

			mInputString.insert(insert_pos, utf32str);

			//If we inserted RtL then the cursor should not be incremented.
			return pos + (char_maybe_rtl ? 0 : utf32str.size());
		}

		//This case is ambiguous.. Can insert non RtL one time amidst RtL characters, but the cursor will be at the end of the insertion, 
		//thus its proximity to RtL will cause the next non-RtL insertion to be placed after the following RtL section. Doing the 'right' thing requires
		//more context than is readily available. Perhaps a special character could be inserted between directional changes to make it more explicit.
		else if (prev_rtl && !char_maybe_rtl)
		{}
		//Have broken out of an RtL section. Need to insert, issue update, and then find where the character ended up in mDisplayString.
		else
		{
			++insert_pos;
			mInputString.insert(insert_pos, utf32str);
			update();
			for (array_size_t i = 0; i < mDisplayToInputArr.size(); ++i)
			{
				if (mDisplayToInputArr[i].first == insert_pos)
					return i + 1;
			}
		}
	}
	else
	{
		if (pos >= mDisplayToInputArr.size() && !char_always_rtl)
		{
			mInputString.insert(0, utf32str);	//Switching to LtR primary.
			return utf32str.size();
		}
		else if (pos && !prev_rtl && !pos_rtl && char_maybe_rtl)
		{
			return pos;
		}
		else
		{
			if (pos && !prev_rtl)
				insert_pos = mDisplayToInputArr[pos - 1].first + 1;
			else if (pos_rtl)
				++insert_pos;
			else if (pos >= mDisplayToInputArr.size())
				insert_pos = 0;

			mInputString.insert(insert_pos, utf32str);
			if (pos && !prev_rtl && char_maybe_rtl)
			{
				update();
				for (array_size_t i = 0; i < mDisplayToInputArr.size(); ++i)
				{
					if (mDisplayToInputArr[i].first == insert_pos)
					{
						return i;
					}
				}
			}
			else
				return pos + (char_maybe_rtl ? 0 : utf32str.size());

		}
	}
	return pos;
}

void CBidiString::replace(string_size_t pos, const llwchar& utf32char)
{
	update();
	mIsDirty = true;

	if (!mDisplayToInputArr.empty())
	{
		llassert_always(mDisplayString.size() == mDisplayToInputArr.size());
		llassert_always(pos < mDisplayToInputArr.size());
		pos = mDisplayToInputArr[pos].first;
	}

	llassert_always(pos < mInputString.size());
	mInputString[pos] = utf32char;
}

const CBidiString::string_size_t CBidiString::removeChar(string_size_t pos, bool backspace)
{
	update();
	mIsDirty = true;

	bool next_rtl = (pos + 1) < mDisplayToInputArr.size() && isRtLCharacter(pos + 1);
	bool cur_rtl = pos < mDisplayToInputArr.size() && isRtLCharacter(pos);
	bool prev_rtl = pos && pos <= mDisplayToInputArr.size() && isRtLCharacter(pos - 1);
	bool prev_prev_rtl = pos > 1 && pos <= mDisplayToInputArr.size() && isRtLCharacter(pos - 2);

	U32 erase_pos = LLWString::npos;
	if (!mIsRtL)
	{
		if (backspace && cur_rtl)
		{
			erase_pos = pos;
		}
		else if (!backspace && prev_rtl)
		{
			erase_pos = --pos;
		}
		else if (pos && backspace && !prev_rtl)
		{
			erase_pos = --pos;
			if (prev_prev_rtl)
			{
				for (U32 i = pos - 1; i < U32_MAX; --i)
				{
					if (isRtLCharacter(i))
						pos = i;
					else
						break;
				}
			}
		}
		else if (!backspace && !cur_rtl)
		{
			erase_pos = pos;
			for (U32 i = pos + 1; i < mDisplayToInputArr.size(); ++i)
			{
				if (isRtLCharacter(i))
					pos = i;
				else
					break;
			}
		}
	}
	else
	{
		if (pos && backspace && !prev_rtl)
		{
			erase_pos = --pos;
		}
		else if (!backspace && !cur_rtl)
		{
			erase_pos = pos;
		}
		else if (!backspace && prev_rtl)
		{
			erase_pos = --pos;
			if (!prev_prev_rtl && pos)
			{
				for (U32 i = pos - 1; i < U32_MAX; --i)
				{
					if (!isRtLCharacter(i))
						pos = i;
					else
						break;
				}
			}
		}
		else if (backspace && cur_rtl)
		{
			erase_pos = pos;
			if (!next_rtl)
			{
				for (U32 i = pos + 1; i < mDisplayToInputArr.size(); ++i)
				{
					if (!isRtLCharacter(i))
						pos = i;
					else
						break;
				}
			}
		}
	}

	if (erase_pos < mDisplayToInputArr.size())
		erase_pos = mDisplayToInputArr[erase_pos].first;

	if (erase_pos >= mInputString.size())
	{
		llinfos << "returning npos" << llendl;
		return LLWString::npos;
	}

	mInputString.erase(erase_pos, 1);
	return pos;
}