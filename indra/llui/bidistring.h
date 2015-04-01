
#ifndef CBIDISTRING_H
#define CBIDISTRING_H
#include "llstring.h"

class CBidiString
{
public:
	typedef std::vector<std::pair<U32, bool> >::size_type array_size_t;
	typedef LLWString::size_type string_size_t;

	CBidiString() : mIsDirty(false), mIsRtL(false), mIsLastCharRtL(false) {}

	CBidiString(const std::string& utf8str) : mIsDirty(false), mIsRtL(false), mIsLastCharRtL(false)
	{
		assign(utf8str);
	}

	CBidiString(const LLWString& utf32str) : mIsDirty(false), mIsRtL(false), mIsLastCharRtL(false)
	{
		assign(utf32str);
	}

	LLWString substr(string_size_t start = 0, string_size_t count = LLWString::npos);

	const string_size_t erase(string_size_t start = 0, LLWString::size_type count = LLWString::npos);

	const string_size_t insert(string_size_t pos, const LLWString& utf32str);

	const string_size_t removeChar(string_size_t pos, bool backspace);

	void replace(string_size_t pos, const llwchar& utf32char);

	const LLWString& getDisplayString()
	{
		update();
		return !mDisplayString.empty() ? mDisplayString : mInputString;
	}

	const LLWString& getString() const
	{
		return mInputString;
	}

	void clear()
	{
		mIsDirty = true;
		mDisplayToInputArr.clear();
		mDisplayString.clear();
		mInputString.clear();
	}

	void assign(const LLWString& utf32str)
	{
		if (utf32str == mInputString)
			return;
		mIsDirty = true;
		mInputString = utf32str;
	}

	void assign(const std::string& utf8str)
	{
		assign(utf8str_to_wstring(utf8str));
	}

	const string_size_t size() const
	{
		return mInputString.size();
	}

	const string_size_t length() const
	{
		return mInputString.length();
	}

	const CBidiString& operator=(const std::string& utf8str)
	{
		assign(utf8str);
		return *this;
	}
	const CBidiString& operator=(const LLWString& utf32str)
	{
		assign(utf32str);
		return *this;
	}

	operator const LLWString&() { return getDisplayString(); }
	
	const bool isRtLCharacter(U32 idx) const
	{
		return mDisplayToInputArr[idx].second;
	}

private:
	void update()
	{
		if (!mIsDirty)
			return;
		mIsDirty = false;
		mIsRtL = false;
		mIsLastCharRtL = false;
		mDisplayString.clear();
		mDisplayToInputArr.clear();
		if (mDisplayString.capacity() < mInputString.size())
			mDisplayString.reserve(mInputString.size());
		if (mDisplayToInputArr.capacity() < mInputString.size())
			mDisplayToInputArr.reserve(mInputString.size());

		if (!mInputString.empty())
			generateDisplayString();
	}

	void generateDisplayString();

	//Some languages (such as arabic) have several glyph variations depending on neighboring characters.
	struct context_data_t
	{
		U32 mGen, mIso, mEnd, mBeg, mMid;
	};

	const context_data_t* getContext(const llwchar wchar);

	std::vector<std::pair<U32, bool> > mDisplayToInputArr;
	LLWString mDisplayString;
	LLWString mInputString;
	bool mIsDirty;
	bool mIsRtL;
	bool mIsLastCharRtL;
};
#endif