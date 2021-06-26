#pragma once

const int SHIP_SIZE = 5; // our ship
const int MARKER_SIZE = 4; // moon in minor / planet in major
const int MIN_PLANET_SIZE = 8; // smallest dot for central body, planet in minor / Sun in major

const int COLOUR_BIT_DEPTH = 256; // we have 256 shades from red to green. Declared as constant for better readability.

const int TOTAL_DATA_POINTS = int(1e4);

const int LIST_ENTRIES_PER_PAGE = 13; // for list displays. Max entries we can fit in a page.

enum TARGETOBJ { SAVETOMAJOR, SAVETOMINOR, SAVETOAUTO };

// Takes in normal RGB triplet, and converts to Orbiter's strange BGR triplet.
DWORD myGetColour(int R, int G, int B)
{
	return B * 256 * 256 + G * 256 + R;
}

DWORD genericColour = myGetColour(150, 157, 154); // based on asteroid (gray)
struct { // colours with help/inspiration from www.schemecolor.com
	DWORD Sun = myGetColour(255, 201, 76);

	DWORD Mercury = myGetColour(151, 151, 159);
	
	DWORD Venus = myGetColour(187, 183, 171);
	DWORD Earth = myGetColour(140, 177, 222);
	DWORD Moon = myGetColour(148, 144, 141);
	DWORD Mars = myGetColour(226, 123, 88);

	DWORD Jupiter = myGetColour(211, 156, 126);
	DWORD Io = myGetColour(180, 92, 61);
	DWORD Europa = myGetColour(210, 207, 218);
	DWORD Saturn = myGetColour(197, 171, 110);
	DWORD Titan = myGetColour(234, 192, 57);
	DWORD Uranus = myGetColour(187, 225, 228);
	DWORD Neptune = myGetColour(96, 129, 255);
} colour;

const int SAVED_COLOURS = 13;
const char SAVED_COLOURS_NAMES[SAVED_COLOURS][8] = { "Sun", "Mercury", "Venus", "Earth", "Moon", "Mars", "Jupiter", "Io", "Europa", "Saturn", "Titan", "Uranus", "Neptune" }; // longest name is 8 bytes
const DWORD SAVED_COLOUR_HEX[SAVED_COLOURS] = { colour.Sun, colour.Mercury, colour.Venus, colour.Earth, colour.Moon, colour.Mars, colour.Jupiter, colour.Io, colour.Europa, colour.Saturn, colour.Titan, colour.Uranus, colour.Neptune };

class TravelMap : public MFD2
{
public:
	TravelMap(DWORD w, DWORD h, VESSEL* vessel);
	~TravelMap();
	char* ButtonLabel(int bt);
	int ButtonMenu(const MFDBUTTONMENU** menu) const;
	bool Update(oapi::Sketchpad* skp);
	static int MsgProc(UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);
	bool ConsumeButton(int bt, int event);
	bool ConsumeKeyBuffered(DWORD key);
	bool ConsumeKeyImmediate(DWORD key, bool newPress); // Should normally have char *kstate as variable, but as this is only called in ConsumeButton, and is not run by Orbiter, I give myself the liberty to give similar function as KeyBuffered.
	void ReadStatus(FILEHANDLE scn);
	void WriteStatus(FILEHANDLE scn) const;

	void DisplayTrack(OBJHANDLE ref, VECTOR3 *pos, int historyIndex, int historyLength, VESSEL *v, oapi::Sketchpad *skp, double *totalDistance);
	void DrawPlanet(OBJHANDLE ref, OBJHANDLE obj, double scale, oapi::Sketchpad* skp, MATRIX3 rot, double offsetX, double offsetY);

	void SaveDataToFiles(void);
	void LoadDataFromFile(int fileIndex);
	void FetchFileNames(char* refName);
	bool ReadDataLineToHistory(char* line, bool flip = true, char separator = ',', TARGETOBJ saveTo = SAVETOAUTO);

	char* SetPlanetBrush(OBJHANDLE obj, oapi::Sketchpad* skp);
	char* GetColourString(DWORD colour);

	char* FormatNumber(double num);
	char* FormatTime(double num);
	void FixCoordinateSystem(VECTOR3& v);

	double MnA2TrA(double MnA, double Ecc);
	double EccentricAnomaly(double ecc, double TrA);

	bool SetMinorReference(char* rstr);
	bool SetMassLimit(char* rstr);
	bool SetRadiusLimit(char* rstr);
private:
	int W, H;
	oapi::Brush* shipBrush;
	oapi::Brush* planetBrush[SAVED_COLOURS];
	oapi::Brush* genericPlanetBrush;
	oapi::Pen* orbitPen;
	oapi::Pen* planetPen;
	oapi::Pen* red;
	oapi::Pen* green;
	oapi::Pen* blue;
	oapi::Pen* gradeRedGreen[COLOUR_BIT_DEPTH];

	bool createSVG = false; // is set to true in one frame, when the figure is created alongside the next MFD update event, and the bool set to false again.
	char SVGname[200];
	char SVGcontent[2 * TOTAL_DATA_POINTS][100];
	int SVGtotalLines = 0;
	char txtPlanets[100][100];
	int txtTotalPlanets = 0;
};

int g_MFDmode; // identifier for new MFD mode

bool showMajor = true;
bool gradedTrajectory = false;
enum TEXTINFO { TEXTNONE, TEXTDIST, TEXTTIME, TEXTLASTENTRY };
TEXTINFO showText = TEXTNONE;
enum PROJECTION { ECLIPTIC, EQUATOR, SELF, MANUAL, LASTENTRY };
PROJECTION proj = ECLIPTIC;
OBJHANDLE SunHandle = oapiGetGbodyByIndex(0);
OBJHANDLE minorHandle;
double massLimit = 2e23; // Ganymede is 1.5e23 kg, Mercury is 3.3e23 kg. So by default only plots the 8 planets.
double displayRadius = -1.0; // negative value is auto (1.1 times max radius).
enum BUTTONPAGE { PAGE1, PAGE2, PAGELOAD , LASTPAGE };
BUTTONPAGE buttPage = PAGE1;

double historyTimeDeltaMajor = 1e5; // gives ~32 years

VECTOR3 majorPos[TOTAL_DATA_POINTS];
VECTOR3 minorPos[TOTAL_DATA_POINTS];
int historyLengthMajor = 0;
int historyLengthMinor = 0;
int historyIndexMajor = 0;
int historyIndexMinor = 0;
double historyLastTimeSaveMajor = -INFINITY;
double historyLastTimeSaveMinor = -INFINITY;

double manInc = 0.0;
double manLan = 0.0;
double immediateKeyStart = 0.0;

bool centreSelf = false;
bool displayZoomShip = false;

// Loading screen
int loadSelection = 0;
int totalLoadChoices = 1; // should never be zero, as it leads to division by zero in modulo.
int currentLoadSelectionTopListed = 0;
const int TOTAL_LOAD_FILES = 1000;
char availableSaves[TOTAL_LOAD_FILES][80]; // a file name is typically 35 characters, plus 16 in folder path.

TravelMap::TravelMap(DWORD w, DWORD h, VESSEL* vessel)
	: MFD2(w, h, vessel)
{
	W = w;
	H = h;
	VESSEL *v = vessel;

	// If invalid Sun handle (could happen after sim exit and restart
	if (oapiGetObjectType(SunHandle) != OBJTP_STAR) SunHandle = oapiGetGbodyByIndex(0);

	// If invalid minor handle
	if (oapiGetObjectType(minorHandle) != OBJTP_PLANET)
	{
		if (v->GroundContact()) minorHandle = v->GetSurfaceRef(); // set to landed body
		else if (v->GetGravityRef() != SunHandle) minorHandle = v->GetGravityRef(); // if not landed, and not Sun as gravity ref
		else if (v->GetSurfaceRef() != SunHandle) minorHandle = v->GetSurfaceRef(); // if not landed, and not Sun as surface ref
		else // if sun as surface ref and gravity ref
		{
			OBJHANDLE earthHandle = oapiGetGbodyByName("Earth");
			if (oapiGetObjectType(earthHandle) == OBJTP_PLANET) minorHandle = earthHandle; // Earth exists, set as surface ref
			else minorHandle = oapiGetGbodyByIndex(1); // backup solution: set to any other G-body.
		}
	}

	shipBrush = oapiCreateBrush(0x2135C5); // C53521 is the colour of the DeltaGlider
	genericPlanetBrush = oapiCreateBrush(genericColour);

	for (int i = 0; i < SAVED_COLOURS; i++)
	{
		planetBrush[i] = oapiCreateBrush(SAVED_COLOUR_HEX[i]);
	}

	orbitPen = oapiCreatePen(1, 1, 0x00FF00);
	planetPen = oapiCreatePen(1, 1, 0x666666);
	/*red = oapiCreatePen(1, 1, 0x0000FF);
	green = oapiCreatePen(1, 1, 0x00FF00);
	blue = oapiCreatePen(1, 1, 0xFF0000);*/
	// Contrast of blue on black is a bit dark, so switch to CMYK
	red = oapiCreatePen(1, 1, 0xFFFF00); // cyan
	green = oapiCreatePen(1, 1, 0xFF00FF); // magenta
	blue = oapiCreatePen(1, 1, 0x00FFFF); // yellow

	// We create all different shades (256 pens), as Orbiter gets buggy when creating and deleting 256 pens every frame. (sometimes they all turn out red)
	for (int i = 0; i < COLOUR_BIT_DEPTH; i++)
	{
		gradeRedGreen[i] = oapiCreatePen(1, 1, myGetColour(int(sqrt(255 * 255 - i * i)), i, 0)); // see this video for why I square and sqrt the colour: https://www.youtube.com/watch?v=LKnqECcg6Gw (TL;DW: brightness)
	}
}

inline TravelMap::~TravelMap()
{
	for (int i = 0; i < SAVED_COLOURS; i++)
	{
		oapiReleaseBrush(planetBrush[i]);
	}

	oapiReleaseBrush(shipBrush);
	oapiReleaseBrush(genericPlanetBrush);
	oapiReleasePen(orbitPen);
	oapiReleasePen(planetPen);
	oapiReleasePen(red);
	oapiReleasePen(green);
	oapiReleasePen(blue);

	for (int i = 0; i < COLOUR_BIT_DEPTH; i++)
	{
		oapiReleasePen(gradeRedGreen[i]);
	}

	char* RGB = GetColourString(0x0);
	delete RGB;
}

const int NUM_BUTTONS_PG1 = 8;
const int NUM_BUTTONS_PG1_MANUAL = NUM_BUTTONS_PG1 + 4;
const int NUM_BUTTONS_PG2 = 8;
const int NUM_BUTTONS_PGLOAD = 6;
inline char* TravelMap::ButtonLabel(int bt)
{
	static char* labelPg1[NUM_BUTTONS_PG1] =			{ "DSP",  "PG", "TXT", "RMIN", "PRJ", "GRD", "CNTR", "RAD" };
	static char* labelPg1M[NUM_BUTTONS_PG1_MANUAL] =	{ "DSP", "PG", "TXT", "RMIN", "PRJ", "GRD", "CNTR", "RAD" , "UP", "DN", "<", ">" };
	static char* labelPg2[NUM_BUTTONS_PG2] =			{ "DSP", "PG", "MASS", "SAV", "LOAD", "-x", "x-", "CLR" };
	static char* labelPgLoad[NUM_BUTTONS_PGLOAD] =		{ "UP", "DN", "SEL", "BCK", "DSP", "RMIN" }; // allow to switch reference in load screen
	
	switch (buttPage)
	{
	case PAGE2:
		return (bt < NUM_BUTTONS_PG2 ? labelPg2[bt] : 0);
	case PAGELOAD:
		return (bt < NUM_BUTTONS_PGLOAD ? labelPgLoad[bt] : 0);
	case PAGE1:
	case LASTPAGE:
	default:
		if (proj == MANUAL) return (bt < NUM_BUTTONS_PG1_MANUAL ? labelPg1M[bt] : 0);
		else return (bt < NUM_BUTTONS_PG1 ? labelPg1[bt] : 0);
	}
}

inline int TravelMap::ButtonMenu(const MFDBUTTONMENU** menu) const
{
	static const MFDBUTTONMENU mnuPg1[NUM_BUTTONS_PG1] = {
		{"Major/minor body", 0, 'D'},
		{"Next page", 0, 'N'},
		{"Distance info", 0, 'T'},
		{"Minor reference", 0, 'R'},
		{"Toggle projection", 0, 'P'},
		{"Graded trajectory", 0, 'G'},
		{"Toggle map centre", 0, 'X'},
		{"Display radius", 0, 'A'},
	};

	static const MFDBUTTONMENU mnuPg1M[NUM_BUTTONS_PG1_MANUAL] = {
		{"Major/minor body", 0, 'D'},
		{"Next page", 0, 'N'},
		{"Distance info", 0, 'T'},
		{"Minor reference", 0, 'R'},
		{"Toggle projection", 0, 'P'},
		{"Graded trajectory", 0, 'G'},
		{"Toggle map centre", 0, 'X'},
		{"Display radius", 0, 'A'},
		{"Pan up", 0, '-'},
		{"Pan down", 0, '='},
		{"Pan left", 0, '['},
		{"Pan right", 0, ']'},
	};

	static const MFDBUTTONMENU mnuPg2[NUM_BUTTONS_PG2] = {
		{"Major/minor body", 0, 'D'},
		{"Previous page", 0, 'N'},
		{"Mass limit for planets", 0, 'M'},
		{"Save to file", 0, 'S'},
		{"Load from file", 0, 'L'},
		{"Remove last point", 0,'O'},
		{"Remove first point", 0, 'F'},
		{"Clear data", 0, 'C'},
	};

	static const MFDBUTTONMENU mnuPgLoad[NUM_BUTTONS_PGLOAD] = {
		{"Previous file", 0, '1'},
		{"Next file", 0, '2'},
		{"Select file", 0, '3'},
		{"Go back", 0, '4'},
		{"Major/minor body", 0, 'D'},
		{"Minor reference", 0, 'R'},
	};

	if (menu)
	{
		switch (buttPage)
		{
		case PAGE2:
			*menu = mnuPg2;
			return NUM_BUTTONS_PG2;
		case PAGELOAD:
			*menu = mnuPgLoad;
			return NUM_BUTTONS_PGLOAD;
		case PAGE1:
		case LASTPAGE:
		default:
			*menu = mnuPg1;
			return NUM_BUTTONS_PG1;
		}
	}
	return 0;
}

inline bool TravelMap::ConsumeButton(int bt, int event)
{
	switch (buttPage)
	{
	case PAGE2:
		// If nothing happens, or not a keydown event, then don't do anything
		if (!(event & PANEL_MOUSE_LBDOWN)) return false;

		if (bt == 0) return ConsumeKeyBuffered(OAPI_KEY_D);
		if (bt == 1) return ConsumeKeyBuffered(OAPI_KEY_N);
		if (bt == 2) return ConsumeKeyBuffered(OAPI_KEY_M);
		if (bt == 3) return ConsumeKeyBuffered(OAPI_KEY_S);
		if (bt == 4) return ConsumeKeyBuffered(OAPI_KEY_L);
		if (bt == 5) return ConsumeKeyBuffered(OAPI_KEY_O);
		if (bt == 6) return ConsumeKeyBuffered(OAPI_KEY_O);
		if (bt == 7) return ConsumeKeyBuffered(OAPI_KEY_C);

		break;
	case PAGELOAD:
		// If nothing happens, or not a keydown event, then don't do anything
		if (!(event & PANEL_MOUSE_LBDOWN)) return false;

		// Normal key down events
		if (bt == 0) return ConsumeKeyBuffered(OAPI_KEY_1);
		if (bt == 1) return ConsumeKeyBuffered(OAPI_KEY_2);
		if (bt == 2) return ConsumeKeyBuffered(OAPI_KEY_3);
		if (bt == 3) return ConsumeKeyBuffered(OAPI_KEY_4);
		if (bt == 4) return ConsumeKeyBuffered(OAPI_KEY_D);
		if (bt == 5) return ConsumeKeyBuffered(OAPI_KEY_R);

		break;
	case PAGE1:
	case LASTPAGE:
	default:
		// The manual left/right/up/down buttons are immediate, so process entire pressed event
		if (proj == MANUAL)
		{
			bool newPress = false;
			if (event & PANEL_MOUSE_LBDOWN) newPress = true;
			else if (event & PANEL_MOUSE_LBPRESSED) newPress = false;
			else return false;

			if (bt == 8) return ConsumeKeyImmediate(OAPI_KEY_MINUS, newPress);
			if (bt == 9) return ConsumeKeyImmediate(OAPI_KEY_EQUALS, newPress);
			if (bt == 10) return ConsumeKeyImmediate(OAPI_KEY_LBRACKET, newPress);
			if (bt == 11) return ConsumeKeyImmediate(OAPI_KEY_RBRACKET, newPress);
		}

		// If nothing happens, or not a keydown event, then don't do anything
		if (!(event & PANEL_MOUSE_LBDOWN)) return false;

		// Normal key down events
		if (bt == 0) return ConsumeKeyBuffered(OAPI_KEY_D);
		if (bt == 1) return ConsumeKeyBuffered(OAPI_KEY_N);
		if (bt == 2) return ConsumeKeyBuffered(OAPI_KEY_T);
		if (bt == 3) return ConsumeKeyBuffered(OAPI_KEY_R);
		if (bt == 4) return ConsumeKeyBuffered(OAPI_KEY_P);
		if (bt == 5) return ConsumeKeyBuffered(OAPI_KEY_G);
		if (bt == 6) return ConsumeKeyBuffered(OAPI_KEY_X);
		if (bt == 7) return ConsumeKeyBuffered(OAPI_KEY_A);

		break;
	}
	
	return false;
}

inline bool TravelMap::ConsumeKeyBuffered(DWORD key)
{
	bool InputMinorReference(void* id, char* str, void* data);
	bool InputMassLimit(void* id, char* str, void* data);
	bool InputDisplayRadius(void* id, char* str, void* data);

	switch (key)
	{
	case OAPI_KEY_D:
		showMajor = !showMajor;
		InvalidateDisplay(); // update screen
		return true;
	case OAPI_KEY_T:
		showText = TEXTINFO((int(showText) + 1) % int(TEXTLASTENTRY));
		return true;
	case OAPI_KEY_R:
		oapiOpenInputBox("Set minor reference body:", InputMinorReference, 0, 20, (void*)this);
		InvalidateDisplay(); // update screen
		return true;
	case OAPI_KEY_P:
		proj = PROJECTION((int(proj) + 1) % int(LASTENTRY));
		InvalidateButtons(); // allow new buttons to pop up or dissapear
		return true;
	case OAPI_KEY_G:
		gradedTrajectory = !gradedTrajectory;
		return true;
	case OAPI_KEY_N:
		if (buttPage == PAGE1) buttPage = PAGE2;
		else buttPage = PAGE1;

		InvalidateButtons(); // switch buttons
		return true;
	case OAPI_KEY_X:
		centreSelf = !centreSelf;

		InvalidateDisplay(); // update screen
		return true;
	case OAPI_KEY_O:
		if (showMajor)
		{
			historyIndexMajor = (historyIndexMajor - 1 + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;
			historyLengthMajor = max(0, historyLengthMajor - 1);
		}
		else
		{
			historyIndexMinor = (historyIndexMinor - 1 + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;
			historyLengthMinor = max(0, historyLengthMinor - 1);
		}
		return true;
	case OAPI_KEY_F:
		if (showMajor) historyLengthMajor = max(0, historyLengthMajor - 1);
		else historyLengthMinor = max(0, historyLengthMinor - 1);

		return true;
	case OAPI_KEY_C:
		if (showMajor) historyLengthMajor = 0;
		else historyLengthMinor = 0;
		return true;
	case OAPI_KEY_M:
		oapiOpenInputBox("Set planet mass limit ('def' = default):", InputMassLimit, 0, 20, (void*)this);
		return true;
	case OAPI_KEY_A:
		oapiOpenInputBox("Set display radius limit (negative value = auto):", InputDisplayRadius, 0, 20, (void*)this);
		return true;
	case OAPI_KEY_S:
		SaveDataToFiles();
		return true;
	case OAPI_KEY_L:
		buttPage = PAGELOAD;

		// Initialise selection
		loadSelection = 0;
		currentLoadSelectionTopListed = 0;

		InvalidateButtons(); // new buttons
		InvalidateDisplay(); // and new screen
		return true;
	case OAPI_KEY_1:
		loadSelection = (loadSelection - 1 + totalLoadChoices) % totalLoadChoices;
		if (loadSelection < currentLoadSelectionTopListed) currentLoadSelectionTopListed = loadSelection;
		if (loadSelection > currentLoadSelectionTopListed + LIST_ENTRIES_PER_PAGE) currentLoadSelectionTopListed = loadSelection - LIST_ENTRIES_PER_PAGE;

		InvalidateDisplay(); // update screen
		return true;
	case OAPI_KEY_2:
		loadSelection = (loadSelection + 1) % totalLoadChoices;
		if (loadSelection < currentLoadSelectionTopListed) currentLoadSelectionTopListed = loadSelection;
		if (loadSelection > currentLoadSelectionTopListed + LIST_ENTRIES_PER_PAGE) currentLoadSelectionTopListed = loadSelection - LIST_ENTRIES_PER_PAGE;

		InvalidateDisplay(); // update screen
		return true;
	case OAPI_KEY_3:
		LoadDataFromFile(loadSelection);

		buttPage = PAGE2;
		InvalidateButtons(); // new buttons
		InvalidateDisplay(); // and new screen
		return true;
	case OAPI_KEY_4:
		buttPage = PAGE2; // can only access loading page from page 2, so send back.

		InvalidateButtons(); // new buttons
		InvalidateDisplay(); // and new screen
		return true;
	}
	return false;
}

inline bool TravelMap::ConsumeKeyImmediate(DWORD key, bool newPress)
{
	double syst = oapiGetSysTime();
	double sysdt = oapiGetSysStep();

	if (newPress) immediateKeyStart = syst;

	double angDelta = 1.0 * RAD + (syst - immediateKeyStart) * 7.5 * RAD; // speed up motion over time
	angDelta = min(angDelta, 45.0 * RAD); // don't allow faster than 45 deg/s.

	switch (key)
	{
	case OAPI_KEY_MINUS:
		manInc = manInc + angDelta * sysdt;
		if (manInc > 0.0) manInc = 0.0;
		break;
	case OAPI_KEY_EQUALS:
		manInc = manInc - angDelta * sysdt;
		if (manInc < -PI) manInc = -PI;
		break;
	case OAPI_KEY_LBRACKET:
		manLan = normangle(manLan + angDelta * sysdt);
		break;
	case OAPI_KEY_RBRACKET:
		manLan = normangle(manLan - angDelta * sysdt);
		break;
	}

	return false; // Don't return anything, unless we want to inhibit Orbiter key handling. Which we don't need.
}

inline void TravelMap::ReadStatus(FILEHANDLE scn)
{
	char* cbuf;

	// Clear history, just in case.
	historyIndexMajor = 0;
	historyLengthMajor = 0;
	historyLastTimeSaveMajor = -INFINITY;
	
	historyIndexMinor = 0;
	historyLengthMinor = 0;
	historyLastTimeSaveMinor = -INFINITY;

	while (oapiReadScenario_nextline(scn, cbuf))
	{
		if (!_strnicmp(cbuf, "DSP", 3))
		{
			showMajor = bool(atoi(cbuf + 3));
		}
		else if (!_strnicmp(cbuf, "DST", 3))
		{
			showText = TEXTINFO(bool(atoi(cbuf + 3)));
		}
		else if (!_strnicmp(cbuf, "RMI", 3))
		{
			OBJHANDLE scnRef = oapiGetGbodyByName(cbuf + 3);
			if (oapiGetObjectType(scnRef) == OBJTP_PLANET) minorHandle = scnRef;
		}
		else if (!_strnicmp(cbuf, "MAS", 3))
		{
			massLimit = atof(cbuf + 3);
		}
		else if (!_strnicmp(cbuf, "RAD", 3))
		{
			displayRadius = atof(cbuf + 3);
		}
		else if (!_strnicmp(cbuf, "PRJ", 3))
		{
			proj = PROJECTION(atoi(cbuf + 3));
		}
		else if (!_strnicmp(cbuf, "MAN", 3))
		{
			// An Inc LAN pair
			char* strPos;
			char* rstr = cbuf + 4;
			strPos = strchr(rstr, ' ');

			if (strPos != NULL) // found two values, i.e. a coordinate in 'inc lan' format, both in degrees.
			{
				manInc = atof(rstr) * RAD;
				manLan = atof(rstr + int(strPos - rstr + 1)) * RAD;
			}
		}
		else if (!_strnicmp(cbuf, "MPOS", 3))
		{
			// e.g. "MPOS 6373800.94 1404981.89 -1081920.19"
			cbuf = cbuf + 5; // remove "MPOS "
			ReadDataLineToHistory(cbuf, false, ' ', SAVETOMAJOR); // false: don't swap. ' ': values are separated by space.
		}
		else if (!_strnicmp(cbuf, "mPOS", 3))
		{
			// e.g. "mPOS 6373800.94 1404981.89 -1081920.19"
			cbuf = cbuf + 5; // remove "mPOS "
			ReadDataLineToHistory(cbuf, false, ' ', SAVETOMINOR); // false: don't swap. ' ': values are separated by space.
		}
		else if (!_strnicmp(cbuf, "END_MFD", 7))
		{
			// For some strange reason, Orbiter only stops oapiReadScenario_nextline when "END" is reached, but a MFD stops with "END_MFD", so tell it to stop manually.

			// But first fix loaded history.
			// I have done something strange, by saying the index is -1 of the length, so fix here.
			if (showMajor)
			{
				if (historyLengthMajor != 0) historyIndexMajor= (historyIndexMajor - 1 + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;
			}
			else
			{
				if (historyLengthMinor != 0) historyIndexMinor = (historyIndexMinor - 1 + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;
			}

			return;
		}
	}
}

inline void TravelMap::WriteStatus(FILEHANDLE scn) const
{
	char cbuf[100];

	oapiWriteScenario_int(scn, "DSP", showMajor);
	oapiWriteScenario_int(scn, "DST", int(showText));

	oapiGetObjectName(minorHandle, cbuf, 50);
	oapiWriteScenario_string(scn, "RMI", cbuf);

	sprintf(cbuf, "%e", massLimit);
	oapiWriteScenario_string(scn, "MAS", cbuf);

	sprintf(cbuf, "%e", displayRadius);
	oapiWriteScenario_string(scn, "RAD", cbuf);

	oapiWriteScenario_int(scn, "PRJ", int(proj));

	if (proj == MANUAL)
	{
		sprintf(cbuf, "%.1f %.1f", manInc * DEG, manLan * DEG);
		oapiWriteScenario_string(scn, "MAN", cbuf);
	}

	// Attempt to save position log. But limit to only every 10 points, which means max 10 lines.
	int historyLength = historyLengthMajor;
	int CompressionRatio = historyLength > 1000 ? 10 : 1;
	int historyIndex = historyIndexMajor;
	for (int i = 0; i < historyLength / CompressionRatio; i++)
	{
		int idx = (historyIndex - historyLength + 1 + i * CompressionRatio + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;
		oapiWriteScenario_vec(scn, "MPOS", majorPos[idx]);
	}

	historyLength = historyLengthMinor;
	CompressionRatio = historyLength > 1000 ? 10 : 1;
	historyIndex = historyIndexMinor;
	for (int i = 0; i < historyLength / CompressionRatio; i++)
	{
		int idx = (historyIndex - historyLength + 1 + i * CompressionRatio + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;
		oapiWriteScenario_vec(scn, "mPOS", minorPos[idx]);
	}
}

inline bool TravelMap::SetMinorReference(char* rstr)
{
	OBJHANDLE myRef = oapiGetGbodyByName(rstr);

	if (myRef == SunHandle)
	{
		showMajor = true;
		return true;
	}
	else if (myRef == NULL)
	{
		return false;
	}
	else
	{
		if (minorHandle != myRef) historyLengthMinor = 0; // clear data, as we're setting a new reference

		minorHandle = myRef;
		showMajor = false; // assume that user wants to see the object (s)he just set.
		return true;
	}
}

inline bool TravelMap::SetMassLimit(char* rstr)
{
	if (strcmp(rstr, "def") == 0) massLimit = 2e23; // Ganymede is 1.5e23 kg, Mercury is 3.3e23 kg.
	else massLimit = atof(rstr);
	return true;
}

inline bool TravelMap::SetRadiusLimit(char* rstr)
{
	if (strchr(rstr, 'x') != NULL || strchr(rstr, 'X') != NULL) displayZoomShip = true;
	else
	{
		displayRadius = atof(rstr);
		displayZoomShip = false;
	}
	return true;
}

bool InputMinorReference(void* id, char* str, void* data)
{
	return ((TravelMap*)data)->SetMinorReference(str);
}

bool InputMassLimit(void* id, char* str, void* data)
{
	return ((TravelMap*)data)->SetMassLimit(str);
}

bool InputDisplayRadius(void* id, char* str, void* data)
{
	return ((TravelMap*)data)->SetRadiusLimit(str);
}

int TravelMap::MsgProc(UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam)
{
	switch (msg) {
	case OAPI_MSG_MFD_OPENED:
		// Our new MFD mode has been selected, so we create the MFD and
		// return a pointer to it.
		return (int)(new TravelMap(LOWORD(wparam), HIWORD(wparam), (VESSEL*)lparam));
	}
	return 0;
}

DLLCLBK void InitModule(HINSTANCE hDLL)
{
	MFDMODESPECEX spec;
	spec.name = "Travelmap";
	spec.key = OAPI_KEY_T;                // MFD mode selection key
	spec.context = NULL;
	spec.msgproc = TravelMap::MsgProc;  // MFD mode callback function

	// Register the new MFD mode with Orbiter
	g_MFDmode = oapiRegisterMFDMode(spec);
}

DLLCLBK void ExitModule(HINSTANCE hDLL)
{
	// Unregister the custom MFD mode when the module is unloaded
	oapiUnregisterMFDMode(g_MFDmode);
}