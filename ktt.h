/*------------------------------------------------------
//		Key translate table
//
// On modern keyboards it is common for ancillary keys
// to generate multi-byte codes.  This header file
// describes a table for translating the mult-byte
// codes into things understood by CP/M.
//
//-----------------------------------------------------*/

struct _ci
{
    BYTE
        queue[128];
    int
	size,
	index;
};

extern int keyTrans(ui32 key, struct _ci *ci);

extern int ktt_load(char *keyfile);

extern char *ktt_name();

extern int ktt_elements();

enum special_keys
{
    F1 = 0x0200, F2, F3, F4, F5, F6, F7, F8, F9, F10,
    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,
    Insert, Delete, Home, End, PageUp, PageDown,
    Up, Down, Right, Left, NP5, ReverseTab,
    SysRq = 0xFF,			/* jrs 2015-01-24 */
    Ignore = 0xFFFF
};

/* KT_UNICODE generates a warning.  Nevertheless, the value is correct.
// KT_LITERAL is a place-holder.  */

enum key_modifiers
{
    KT_UNICODE = 0x08000000,
    KT_ALT     = 0x40000000,
    KT_CONTROL = 0x20000000,
    KT_SHIFT   = 0x10000000,
    KT_LITERAL = 0x00000000
};

