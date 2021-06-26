/*
	A MFD that displays the total trajectory for your flight.
	Similar to this: https://commons.wikimedia.org/wiki/File:Dawn_trajectory_as_of_September_2009.png
	I will also use the opportunity to do a simple calculation of total distance travelled.

	As I need constant tracking, I need opcPreStep. Therefore, I must declare which spacecraft to track. 
	With Multistage rockets that spawn probes after launch, this can be tricky. 
	Maybe it's best to simply record the current focus object, and hope that the user doesn't follow any other objects.

	Also, I hope to also use this for Earth-Moon trajectories, which then would need coordinates relative to the Earth.
	While normal gravity assist missions are relative to the Sun.
	I will then have to get the user to specify relative object at sim start. Or maybe have two logs: one for Sun and
	one for surface reference at sim start (should be possible to override with button).

	But in general, this MFD sadly needs the user to plan at sim start what to do for the sim session.

	Addon by Asbjørn 'asbjos' Krüger, 2020.
	This source code is released under a Creative Commons Attribution - NonCommercial - ShareAlike 4.0 International License.
	For other uses, please contact me (I'm username 'asbjos' on Orbiter-Forum).
*/

#define STRICT
#define ORBITER_MODULE

#include "Orbitersdk.h"
#include "TravelMapMFD.h"
#include <time.h> // to get time for saves
#define _SILENCE_EXPERIMENTAL_FILESYSTEM_DEPRECATION_WARNING
#include <experimental/filesystem> // to load saves
namespace fs = std::experimental::filesystem;

DLLCLBK void opcPreStep(double simt, double simdt, double mjd)
{
	VESSEL* v = oapiGetFocusInterface();

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


	if (historyLengthMajor == 0) // allow landed, as you're probably not landed on the Sun.
	{
		v->GetRelativePos(SunHandle, majorPos[historyIndexMajor]);
		historyLastTimeSaveMajor = simt;
		historyLengthMajor = 1; // i.e. ++
	}

	if (historyLengthMinor == 0 && !((v->GroundContact() && v->GetSurfaceRef() == minorHandle))) // we still allow points to be sampled if landed on the moon, but ref. Earth, for example.
	{
		v->GetRelativePos(minorHandle, minorPos[historyIndexMinor]);
		historyLastTimeSaveMinor = simt;
		historyLengthMinor = 1; // i.e. ++
	}

	const double minorRadius = oapiGetSize(minorHandle);
	const double mR = 2.0 * minorRadius; // orbit with radius twice of planet
	const double mM = oapiGetMass(minorHandle);
	const double minorOrbitPeriod = PI2 * sqrt(mR * mR * mR / mM / GGRAV);
	const double legacyMinorHistoryDelta = minorOrbitPeriod / 20.0; // Jupiter: 170 days, Earth: 83 days

	VECTOR3 currentMinorPos, currentMinorVel;
	v->GetRelativePos(minorHandle, currentMinorPos);
	v->GetRelativeVel(minorHandle, currentMinorVel);

	const double timeFactor = (6371e3 + 250e3) / (8e3) / 5500.0 * 100.0; // last number is number of saves per LEO orbit.
	double minorHistoryDelta = min(legacyMinorHistoryDelta, length(currentMinorPos) / length(currentMinorVel) / timeFactor); // empirical formula giving smooth track for LEO.

	if (simt > historyLastTimeSaveMinor + minorHistoryDelta && !(v->GroundContact() && v->GetSurfaceRef() == minorHandle)) // we still allow points to be sampled if landed on the moon, but ref. Earth, for example.
	{
		historyIndexMinor = (historyIndexMinor + 1) % TOTAL_DATA_POINTS;
		v->GetRelativePos(minorHandle, minorPos[historyIndexMinor]);
		historyLastTimeSaveMinor = simt;

		if (historyLengthMinor < TOTAL_DATA_POINTS) historyLengthMinor++;
	}

	if (simt > historyLastTimeSaveMajor + historyTimeDeltaMajor)
	{
		historyIndexMajor = (historyIndexMajor + 1) % TOTAL_DATA_POINTS;
		v->GetRelativePos(SunHandle, majorPos[historyIndexMajor]);
		historyLastTimeSaveMajor = simt;

		if (historyLengthMajor < TOTAL_DATA_POINTS) historyLengthMajor++;
	}
}

inline bool TravelMap::Update(oapi::Sketchpad* skp)
{
	char cbuf[256];
	int x0 = W / 40;
	int y0 = H / 20;
	VESSEL* v = oapiGetFocusInterface();

	char refName[30];
	oapiGetObjectName(showMajor ? SunHandle : minorHandle, refName, 30);
	sprintf(cbuf, "Travel map: %s", refName);
	Title(skp, cbuf);

	if (buttPage == PAGELOAD) // display load screen
	{
		int lineNr = -currentLoadSelectionTopListed;
		int dY = H / 16;

		FetchFileNames(refName);

		for (int i = 0; i < totalLoadChoices; i++) // totalLoadChoices is updated in FetchFileNames().
		{
			int yPos = y0 + dY * lineNr;

			if (0 < yPos && yPos < H) // inside screen, and not overlapping MFD title.
			{
				// file name is in format "Images\Travelmap\[name].txt", so remove path from displaying
				char fileName[50];
				char* lastSlashPosWild = strchr(availableSaves[i], '\\');
				char* lastSlashPos = "q[empty]"; // first character gets removed
				while (lastSlashPosWild != NULL)
				{
					lastSlashPosWild = strchr(lastSlashPosWild + 1, '\\');
					if (lastSlashPosWild != NULL) lastSlashPos = lastSlashPosWild; // save the value if actual position.
				}

				int pathLen = int(lastSlashPos - availableSaves[i]);
				strcpy(fileName, availableSaves[i] + pathLen + 1);

				sprintf(cbuf, fileName);
				skp->Text(x0, yPos, cbuf, strlen(cbuf));
			}

			lineNr++;
		}

		// Create selection box
		skp->SetPen(orbitPen); // re-use old green pen.
		skp->Rectangle(x0 - 5, y0 + (loadSelection - currentLoadSelectionTopListed) * dY, W - x0, y0 + dY + (loadSelection - currentLoadSelectionTopListed) * dY);
	}
	else // display travel map
	{
		switch (proj)
		{
		case ECLIPTIC:
			sprintf(cbuf, "ECL");
			break;
		case EQUATOR:
			sprintf(cbuf, "EQU");
			break;
		case SELF:
			sprintf(cbuf, "SLF");
			break;
		case MANUAL:
			sprintf(cbuf, "MAN");
			break;
		default:
			sprintf(oapiDebugString(), "ERROR! Projection doesn't exist.");
			break;
		}
		skp->Text(35 * x0, 0, cbuf, strlen(cbuf));

		if (createSVG) skp->Text(W / 2, H / 2, "Saving data ...", 16); // flash info text while saving

		double totalDistance = 0.0;

		if (showMajor)
		{
			DisplayTrack(SunHandle, majorPos, historyIndexMajor, historyLengthMajor, v, skp, &totalDistance);
		}
		else
		{
			DisplayTrack(minorHandle, minorPos, historyIndexMinor, historyLengthMinor, v, skp, &totalDistance);
		}

		if (showText == TEXTDIST)
		{
			sprintf(cbuf, "Tot. dist.: %s km", FormatNumber(totalDistance / 1e3));
			skp->Text(x0, y0 * 19, cbuf, strlen(cbuf));
		}
		else if (showText == TEXTTIME)
		{

			const double minorRadius = oapiGetSize(minorHandle);
			const double mR = 2.0 * minorRadius; // orbit with radius twice of planet
			const double mM = oapiGetMass(minorHandle);
			const double minorOrbitPeriod = PI2 * sqrt(mR * mR * mR / mM / GGRAV);
			const double legacyMinorHistoryDelta = minorOrbitPeriod / 20.0; // Jupiter: 170 days, Earth: 83 days

			VECTOR3 currentMinorPos, currentMinorVel;
			v->GetRelativePos(minorHandle, currentMinorPos);
			v->GetRelativeVel(minorHandle, currentMinorVel);

			const double timeFactor = (6371e3 + 250e3) / (8e3) / 5500.0 * 100.0; // last number is number of saves per LEO orbit.
			double minorHistoryDelta = min(legacyMinorHistoryDelta, length(currentMinorPos) / length(currentMinorVel) / timeFactor); // empirical formula giving smooth track for LEO.

			double time;
			if (showMajor) time = TOTAL_DATA_POINTS * historyTimeDeltaMajor;
			else time = minorHistoryDelta * TOTAL_DATA_POINTS;
			sprintf(cbuf, "Tot. time: %s", FormatTime(time));
			skp->Text(x0, y0 * 18, cbuf, strlen(cbuf));

			double percentSpace;
			if (showMajor) percentSpace = double(historyLengthMajor) / double(TOTAL_DATA_POINTS) * 100.0;
			else percentSpace = double(historyLengthMinor) / double(TOTAL_DATA_POINTS) * 100.0;
			sprintf(cbuf, "Space used: %.2f %%", percentSpace);
			skp->Text(x0, y0 * 19, cbuf, strlen(cbuf));
		}
	}

	return true;
}

inline void TravelMap::DisplayTrack(OBJHANDLE ref, VECTOR3 *pos, int historyIndex, int historyLength, VESSEL *v, oapi::Sketchpad *skp, double *totalDistance)
{
	SVGtotalLines = 0;

	VECTOR3 currentPos;
	v->GetRelativePos(ref, currentPos);
	FixCoordinateSystem(currentPos);

	double scale = 1.0;
	if (displayZoomShip) scale = length(currentPos);
	else if (displayRadius <= 0.0) // auto radius
	{
		double maxRad2 = length2(currentPos);
		for (int i = 0; i < historyLength; i++)
		{
			int indexCurrent = (historyIndex - i + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;

			double rad = length2(pos[indexCurrent]);
			if (rad > maxRad2) maxRad2 = rad;
		}

		scale = sqrt(maxRad2);
	}
	else scale = displayRadius;

	MATRIX3 rotMatrix = identity();
	ELEMENTS el;
	switch (proj)
	{
	case EQUATOR:
		oapiGetPlanetObliquityMatrix(ref, &rotMatrix);
		// Translate matrix to right-handed matrix multiplication (original matrix is for left-handed transformed multiplication)
		rotMatrix = _M(
			rotMatrix.m11, rotMatrix.m31, rotMatrix.m21,
			rotMatrix.m13, rotMatrix.m33, rotMatrix.m23,
			rotMatrix.m12, rotMatrix.m32, rotMatrix.m22
		);
		break;
	case SELF:
		v->GetElements(ref, el);
		rotMatrix = rotm(_V(cos(el.theta), sin(el.theta), 0), -el.i);
		break;
	case MANUAL:
		rotMatrix = mul(rotm(_V(1, 0, 0), manInc) , rotm(_V(0, 0, 1), manLan));
		break;
	case ECLIPTIC:
		rotMatrix = identity();
		break;
	default:
		sprintf(oapiDebugString(), "ERROR! Un-obtainable rotation matrix!");
		break;
	}

	// Offset, which makes it possible to centre on ship
	VECTOR3 currPos = currentPos;
	currPos = mul(rotMatrix, currPos);
	VECTOR3 offset = centreSelf ? currPos : _V(0, 0, 0);
	double offsetX = offset.x / scale / 1.1 * W / 2;
	double offsetY = -offset.y / scale / 1.1 * H / 2; // negative, as (0,0) is top left

	// Begin drawing, and if SVG, begin create file.
	if (createSVG)
	{
		sprintf(SVGcontent[SVGtotalLines], "<svg width=\"%i\" height=\"%i\" xmlns=\"http://www.w3.org/2000/svg\">", W, H);
		SVGtotalLines++;
	}

	// Show body
	skp->SetPen(NULL);
	char* colour = SetPlanetBrush(ref, skp);
	const int PLANET_SIZE = max(min(W, oapiGetSize(ref) / scale / 1.1 * W / 2), MIN_PLANET_SIZE); // min(W, ...) to not overflow
	VECTOR3 mainPlanetPos = _V(0, 0, 0) - offset;
	double X = W / 2;
	double Y = H / 2;
	skp->Ellipse(X - PLANET_SIZE - offsetX, Y - PLANET_SIZE - offsetY, X + PLANET_SIZE - offsetX, Y + PLANET_SIZE - offsetY);
	if (createSVG)
	{
		sprintf(SVGcontent[SVGtotalLines], "<circle cx=\"%f\" cy=\"%f\" r=\"%i\" fill=\"%s\"/>", X - offsetX, Y - offsetY, PLANET_SIZE, colour);
		SVGtotalLines++;
	}

	// Draw axis in top right
	if (proj == MANUAL)
	{
		X = W * 9 / 10;
		Y = H * 1 / 10;
		double unitLength = 20;
		VECTOR3 unitX = _V(1, 0, 0);
		VECTOR3 unitY = _V(0, 1, 0);
		VECTOR3 unitZ = _V(0, 0, 1);
		unitX = mul(rotMatrix, unitX);
		unitY = mul(rotMatrix, unitY);
		unitZ = mul(rotMatrix, unitZ);
		skp->SetPen(red);
		skp->Line(X, Y, X + unitX.x * unitLength, Y - unitX.y * unitLength);
		skp->SetPen(green);
		skp->Line(X, Y, X + unitY.x * unitLength, Y - unitY.y * unitLength);
		skp->SetPen(blue);
		skp->Line(X, Y, X + unitZ.x * unitLength, Y - unitZ.y * unitLength);
	}

	// Decide which Gbodies to display
	txtTotalPlanets = 0; // reset planet counter for text
	for (int i = 1; i < (int)oapiGetGbodyCount(); i++)
	{
		OBJHANDLE currentBody = oapiGetGbodyByIndex(i);

		// Gravitational force from the Sun is greater than from the Earth for the Moon. But we all know Moon is orbiting Earth, so manually set parent. This is an inelegant solution, but I can't find any better way
		if (currentBody == oapiGetGbodyByName("Moon"))
		{
			if (!showMajor && ref == oapiGetGbodyByName("Earth")) // if displaying minor, and that minor is Earth, then draw
			{
				DrawPlanet(ref, currentBody, scale, skp, rotMatrix, offsetX, offsetY);
			}
		}
		else
		{
			// We have a generic Gbody. Now find out what it orbits (if moon or planet), and display it if is to be (moon in minor, planet in major)
			double parentForce = 0.0;
			OBJHANDLE parentHandle = SunHandle; // initialise

			for (int k = 0; k < i; k++) // as the Gbody index is ordered in after mass in descending order, we only have to check up to our own index, as nothing orbits a lighter object
			{
				OBJHANDLE parentBody = oapiGetGbodyByIndex(k);
				double parentMass = oapiGetMass(parentBody);
				VECTOR3 parentPos;
				oapiGetRelativePos(currentBody, parentBody, &parentPos);
				double parentDist2 = length2(parentPos);
				double force = parentMass / parentDist2;
				if (parentForce < force) // Parent body is the body with largest gravitational force on selected object
				{
					parentHandle = parentBody;
					parentForce = force;
				}
			}

			if (parentHandle == ref && (!showMajor || oapiGetMass(currentBody) > massLimit)) // parent is the current reference, so display. But must be within mass limit if in major view.
			{
				DrawPlanet(ref, currentBody, scale, skp, rotMatrix, offsetX, offsetY);
			}
		}
	}

	int indexPrevious = (historyIndex - (historyLength - 1) + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS; // the index of the oldest recorded point.
	VECTOR3 prevPos = pos[indexPrevious];
	FixCoordinateSystem(prevPos);
	prevPos = mul(rotMatrix, prevPos);
	double X1 = W / 2 + prevPos.x / scale / 1.1 * W / 2;
	double Y1 = W / 2 - prevPos.y / scale / 1.1 * H / 2; // negative, as (0,0) is top left
	skp->MoveTo(X1 - offsetX, Y1 - offsetY);
	if (createSVG)
	{
		sprintf(SVGcontent[SVGtotalLines], "<polyline points=\"%f,%f ", X1 - offsetX, Y1 - offsetY);
		SVGtotalLines++;
	}
	skp->SetPen(orbitPen); // may be overturned by graded trajectory option

	// If grading, hold count on what index we're at.
	int previousColourGrade = -1; // Set to initial off value, so that we start with fresh pen.

	for (int i = 0; i < historyLength - 1; i++)
	{
		int indexCurrent = (historyIndex - (historyLength - 1) + i + 1 + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;

		VECTOR3 currPos = pos[indexCurrent];
		FixCoordinateSystem(currPos);

		currPos = mul(rotMatrix, currPos);
		double X2 = W / 2 + currPos.x / scale / 1.1 * W / 2;
		double Y2 = W / 2 - currPos.y / scale / 1.1 * H / 2; // negative, as (0,0) is top left

		// Attempt to interpolate quadratic curve between every three points, and calculate arc length. Is a mess, and didn't work well.
		//if (i == 0)
		//{
		//	lastThree[0] = prevPos;
		//	*totalDistance += length(currPos - prevPos);
		//}
		//else if (i % 2 == 1) // sum over last three points, which then includes the last from previous round, i.e. every two points we do this.
		//{
		//	lastThree[1] = prevPos;
		//	lastThree[2] = currPos;

		//	double k2 = -3.0 * lastThree[0].x * lastThree[0].x - 8.0 * lastThree[1].x * lastThree[1].x - lastThree[2].x * lastThree[2].x - 4.0 * lastThree[0].x * lastThree[2].x + 10.0 * lastThree[0].x * lastThree[1].x + 6.0 * lastThree[1].x * lastThree[2].x \
		//		- 3.0 * lastThree[0].y * lastThree[0].y - 8.0 * lastThree[1].y * lastThree[1].y - lastThree[2].y * lastThree[2].y - 4.0 * lastThree[0].y * lastThree[2].y + 10.0 * lastThree[0].y * lastThree[1].y + 6.0 * lastThree[1].y * lastThree[2].y \
		//		- 3.0 * lastThree[0].z * lastThree[0].z - 8.0 * lastThree[1].z * lastThree[1].z - lastThree[2].z * lastThree[2].z - 4.0 * lastThree[0].z * lastThree[2].z + 10.0 * lastThree[0].z * lastThree[1].z + 6.0 * lastThree[1].z * lastThree[2].z;

		//	double k1 = (lastThree[2].x + lastThree[0].x - 2.0 * lastThree[1].x) * (lastThree[2].x + lastThree[0].x - 2.0 * lastThree[1].x) \
		//		+ (lastThree[2].y + lastThree[0].y - 2.0 * lastThree[1].y) * (lastThree[2].y + lastThree[0].y - 2.0 * lastThree[1].y) \
		//		+ (lastThree[2].z + lastThree[0].z - 2.0 * lastThree[1].z) * (lastThree[2].z + lastThree[0].z - 2.0 * lastThree[1].z);

		//	double k3 = ((4.0 * lastThree[1].x - 3.0 * lastThree[0].x - lastThree[2].x) / 2.0) * ((4.0 * lastThree[1].x - 3.0 * lastThree[0].x - lastThree[2].x) / 2.0) \
		//		+ ((4.0 * lastThree[1].y - 3.0 * lastThree[0].y - lastThree[2].y) / 2.0) * ((4.0 * lastThree[1].y - 3.0 * lastThree[0].y - lastThree[2].y) / 2.0) \
		//		+ ((4.0 * lastThree[1].z - 3.0 * lastThree[0].z - lastThree[2].z) / 2.0) * ((4.0 * lastThree[1].z - 3.0 * lastThree[0].z - lastThree[2].z) / 2.0);

		//	double dist = (4.0 * k1 + k2) * sqrt(2.0 * (k1 * 2.0 + k2) + k3) / (4.0 * k1) - (k2 * k2 - 4.0 * k1 * k3) / (8.0 * k1 * sqrt(k1)) * log(2.0 * sqrt(k1) * sqrt(2.0 * (k1 * 2.0 + k2) + k3) + 4.0 * k1 + k2) \
		//		- (k2 * sqrt(k3) / (4.0 * k1) - (k2 * k2 - 4.0 * k1 * k3) / (8.0 * k1 * sqrt(k1)) * log(2.0 * sqrt(k1) * sqrt(k3) + k2));

		//	*totalDistance += dist;
		//	lastThree[0] = prevPos; // re-assign the last point to be the first point next round.
		//}
		//else if (i == historyLength - 2) // last point has not been accounted for above
		//{
		*totalDistance += length(currPos - prevPos);

		int colourGrade = int(255.0 * double(i) / double(historyLength - 2));// only used if gradedTrajectory
		if (historyLength == 2) colourGrade = 0; // division by zero, but then i can only be 0, so set 255 * 0 / 0 = 0

		if (gradedTrajectory && colourGrade != previousColourGrade)
		{
			if (colourGrade < 0 || colourGrade > 255) oapiWriteLogV("ERROR! Travelmap grade off-scale! colourGrade: %i, simt: %.2f, historyLength: %i, i: %i", colourGrade, oapiGetSimTime(), historyLength, i); // debug, should never happen!

			skp->SetPen(gradeRedGreen[colourGrade]); // set the new pen.

			previousColourGrade = colourGrade; // update value

			if (createSVG)
			{
				// End old line with old colour.
				sprintf(SVGcontent[SVGtotalLines], "\" fill=\"none\" stroke=\"%s\" />", GetColourString(myGetColour(int(sqrt(255 * 255 - colourGrade * colourGrade)), colourGrade, 0)));
				SVGtotalLines++;

				// Start new line. Use prevPos to find location of where to start from.
				double X3 = W / 2 + prevPos.x / scale / 1.1 * W / 2;
				double Y3 = W / 2 - prevPos.y / scale / 1.1 * H / 2; // negative, as (0,0) is top left
				sprintf(SVGcontent[SVGtotalLines], "<polyline points=\"%f,%f ", X3 - offsetX, Y3 - offsetY);
				SVGtotalLines++;
			}
		}

		skp->LineTo(X2 - offsetX, Y2 - offsetY);
		if (createSVG)
		{
			sprintf(SVGcontent[SVGtotalLines], "%f,%f ", X2 - offsetX, Y2 - offsetY);
			SVGtotalLines++;
		}

		prevPos = currPos;
	}

	// End line
	if (createSVG)
	{
		sprintf(SVGcontent[SVGtotalLines], "\" fill=\"none\" stroke=\"%s\" />", GetColourString(myGetColour(0, 255, 0))); // endpoint is always green, no matter gradient or not.
		SVGtotalLines++;
	}

	// Draw ship position. CurrPos is created and rotated when setting up offset.
	X = W / 2 + currPos.x / scale / 1.1 * W / 2;
	Y = H / 2 - currPos.y / scale / 1.1 * H / 2; // negative, as (0,0) is top left
	skp->SetBrush(shipBrush);
	skp->Ellipse(X - SHIP_SIZE - offsetX, Y - SHIP_SIZE - offsetY, X + SHIP_SIZE - offsetX, Y + SHIP_SIZE - offsetY);
	if (createSVG)
	{
		sprintf(SVGcontent[SVGtotalLines], "<circle cx=\"%f\" cy=\"%f\" r=\"%i\" fill=\"%s\"/>", X - offsetX, Y - offsetY, SHIP_SIZE, GetColourString(0x2135C5));
		SVGtotalLines++;
	}

	if (createSVG)
	{
		// Now create file, and paste in all content.
		std::ofstream svgFile(SVGname);

		for (int i = 0; i < SVGtotalLines; i++)
		{
			svgFile << SVGcontent[i] << std::endl;
		}

		svgFile << "</svg>" << std::endl; // end file and goodbye.
	}

	createSVG = false; // reset to false, no matter if it was true or false before.
}

void TravelMap::DrawPlanet(OBJHANDLE ref, OBJHANDLE obj, double scale, oapi::Sketchpad* skp, MATRIX3 rot, double offsetX, double offsetY)
{
	double refMu = oapiGetMass(ref) * GGRAV;

	VECTOR3 pos, vel;
	oapiGetRelativePos(obj, ref, &pos);
	oapiGetRelativeVel(obj, ref, &vel);
	FixCoordinateSystem(pos);
	FixCoordinateSystem(vel);

	double radius = length(pos);
	double speed = length(vel);
	double radialSpeed = dotp(pos, vel) / radius;
	VECTOR3 angularMomentumVector = crossp(pos, vel);
	double inclination = acos(angularMomentumVector.z / length(angularMomentumVector));
	VECTOR3 normalVector = crossp(_V(0.0, 0.0, 1.0), angularMomentumVector);
	double LAN = acos(normalVector.x / length(normalVector));
	if (normalVector.y < 0.0)
		LAN = PI2 - LAN;
	VECTOR3 eccentricityVector = (pos * (speed * speed - refMu / radius) - vel * radius * radialSpeed) / refMu;
	double eccentricity = length(eccentricityVector);
	double APe = acos(dotp(unit(normalVector), unit(eccentricityVector)));
	if (eccentricityVector.z < 0.0)
		APe = PI2 - APe;
	double TrA = acos(dotp(eccentricityVector, pos) / eccentricity / radius);
	if (radialSpeed < 0.0)
		TrA = PI2 - TrA;
	double eccentricAnomaly = 2.0 * atan(tan(TrA / 2.0) / sqrt((1.0 + eccentricity) / (1.0 - eccentricity)));
	if (eccentricity > 1.0) eccentricAnomaly = 2.0 * atanh(tan(TrA / 2.0) / sqrt((eccentricity + 1.0) / (eccentricity - 1.0))); // hyperbolic equation of motion

	double SMa = 1.0 / (2.0 / radius - speed * speed / refMu);
	double MnA = eccentricAnomaly - eccentricity * sin(eccentricAnomaly);
	if (eccentricity > 1.0) MnA = eccentricity * sinh(eccentricAnomaly) - eccentricAnomaly;

	// Find elements of orbit
	ELEMENTS el;
	el.a = SMa;
	el.e = eccentricity;
	el.i = inclination;
	el.omegab = posangle(LAN + APe);
	el.L = posangle(el.omegab + MnA);
	el.theta = LAN;

	pos = mul(rot, pos);

	double X = W / 2 + pos.x / scale / 1.1 * W / 2;
	double Y = W / 2 - pos.y / scale / 1.1 * H / 2; // negative, as (0,0) is top left
	skp->MoveTo(X - offsetX, Y - offsetY); // set origin (where the planet is now)
	if (createSVG)
	{
		sprintf(SVGcontent[SVGtotalLines], "<polyline points=\"%f,%f ", X - offsetX, Y - offsetY);
		SVGtotalLines++;
	}

	skp->SetPen(planetPen);
	const int NUM_ORBIT_STEPS = 100;

	for (int i = 0; i < NUM_ORBIT_STEPS; i++)
	{
		double stepMnA = posangle(MnA + PI2 * double(i) / double(NUM_ORBIT_STEPS));

		// First calculate state vector at step
		double TrA = MnA2TrA(stepMnA, el.e);
		// Find state vectors
		double E = EccentricAnomaly(el.e, TrA); // Find eccentric anomaly at the set time
		double r = el.a * (1.0 - el.e * cos(E)); // Find alt at the set time
		VECTOR3 orbitalFramePos = _V(cos(TrA), sin(TrA), 0.0) * r;

		VECTOR3 statePos; // Thank you to https://downloads.rene-schwarz.com/download/M001-Keplerian_Orbit_Elements_to_Cartesian_State_Vectors.pdf
		statePos.x = orbitalFramePos.x * (cos(APe) * cos(LAN) - sin(APe) * cos(el.i) * sin(LAN)) - orbitalFramePos.y * (sin(APe) * cos(LAN) + cos(APe) * cos(el.i) * sin(LAN));
		statePos.y = orbitalFramePos.x * (cos(APe) * sin(LAN) + sin(APe) * cos(el.i) * cos(LAN)) + orbitalFramePos.y * (cos(APe) * cos(el.i) * cos(LAN) - sin(APe) * sin(LAN));
		statePos.z = orbitalFramePos.x * sin(APe) * sin(el.i) + orbitalFramePos.y * cos(APe) * sin(el.i);

		statePos = mul(rot, statePos);

		X = W / 2 + statePos.x / scale / 1.1 * W / 2;
		Y = W / 2 - statePos.y / scale / 1.1 * H / 2; // negative, as (0,0) is top left
		skp->LineTo(X - offsetX, Y - offsetY);
		if (createSVG)
		{
			sprintf(SVGcontent[SVGtotalLines], "%f,%f ", X - offsetX, Y - offsetY);
			SVGtotalLines++;
		}
	}

	X = W / 2 + pos.x / scale / 1.1 * W / 2;
	Y = W / 2 - pos.y / scale / 1.1 * H / 2; // negative, as (0,0) is top left
	skp->LineTo(X - offsetX, Y - offsetY); // back to real position
	if (createSVG)
	{
		sprintf(SVGcontent[SVGtotalLines], "%f,%f\" fill=\"none\" stroke=\"%s\" />", X - offsetX, Y - offsetY, GetColourString(0x666666));
		SVGtotalLines++;
	}

	// And finally our dot
	char *colour = SetPlanetBrush(obj, skp);
	skp->SetPen(NULL);
	skp->Ellipse(X - MARKER_SIZE - offsetX, Y - MARKER_SIZE - offsetY, X + MARKER_SIZE - offsetX, Y + MARKER_SIZE - offsetY); // Draw marker
	if (createSVG)
	{
		sprintf(SVGcontent[SVGtotalLines], "<circle cx=\"%f\" cy=\"%f\" r=\"%i\" fill=\"%s\"/>", X - offsetX, Y - offsetY, MARKER_SIZE, colour);
		SVGtotalLines++;
	}

	// And also create a planet status every step
	sprintf(txtPlanets[txtTotalPlanets], "Planet %s , %f , %f , %e , %f , %f , %f", colour, MnA, el.e, el.a, APe, LAN, el.i);
	txtTotalPlanets++;
}

// Save the data to a SVG graph (using current scale, projection, grading, etc.) and raw text data.
void TravelMap::SaveDataToFiles(void)
{
	// Create file name in format yymmdd_hhmmss_ref_totdist.svg
	char fileName[200];
	// Get time.
	time_t rawtime;
	char currentTime[80];
	time(&rawtime);
	strftime(currentTime, 80, "%g%m%d_%H%M%S", localtime(&rawtime));
	// Get reference.
	char refName[20];
	oapiGetObjectName(showMajor ? SunHandle : minorHandle, refName, 20);
	// Find total distance
	int historyLength = showMajor ? historyLengthMajor : historyLengthMinor;
	int historyIndex = showMajor ? historyIndexMajor : historyIndexMinor;
	double totalDistance = 0.0;
	const int indexPrevious = (historyIndex - (historyLength - 1) + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS; // the index of the oldest recorded point.
	VECTOR3 prevPos = showMajor ? majorPos[indexPrevious] : minorPos[indexPrevious];
	FixCoordinateSystem(prevPos);
	for (int i = 0; i < historyLength - 1; i++)
	{
		int indexCurrent = (historyIndex - (historyLength - 1) + i + 1 + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;

		VECTOR3 currPos = showMajor ? majorPos[indexCurrent] : minorPos[indexCurrent];
		FixCoordinateSystem(currPos);
		totalDistance += length(currPos - prevPos);
		prevPos = currPos;
	}
	// Create directory
	CreateDirectory("Images\\Travelmap", NULL); // will this work on Wine or other non-Windows systems? I don't know.
	// We now have full text file name
	sprintf(fileName, "Images\\Travelmap\\%s_%s_%.3ekm.txt", currentTime, refName, totalDistance / 1e3);
	// Create the file
	std::ofstream txtFile (fileName);
	// Write header with planet info
	txtFile << txtTotalPlanets << std::endl; // how many lines we use for planets
	for (int i = 0; i < txtTotalPlanets; i++)
	{
		txtFile << txtPlanets[i] << std::endl;
	}
	// Write positions
	for (int i = 0; i < historyLength; i++)
	{
		int indexCurrent = (historyIndex - (historyLength - 1) + i + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;
		VECTOR3 currPos = showMajor ? majorPos[indexCurrent] : minorPos[indexCurrent];
		FixCoordinateSystem(currPos);
		txtFile << currPos.x << " , " << currPos.y << " , " << currPos.z << std::endl;
	}

	// And now create a SVG file. How exciting!
	sprintf(SVGname, "Images\\Travelmap\\%s_%s_%.3ekm.svg", currentTime, refName, totalDistance / 1e3);
	createSVG = true; // the SVG is created in DisplayTrack(), as we need the data that is made there. So technically, I believe the SVG is created in the subsequent frame, but that should not be a big problem.
}

void TravelMap::LoadDataFromFile(int fileIndex)
{
	// Open availableSaves[fileIndex], and load data into history.
	FILEHANDLE fileToLoad = oapiOpenFile(availableSaves[fileIndex], FILE_IN_ZEROONFAIL);

	if (fileToLoad != 0) // could open file, so read contents.
	{
		int lineNr = 0;
		char* line;
		bool reading = true;
		int planetLines = 0;

		// Clear history.
		if (showMajor)
		{
			historyIndexMajor = 0;
			historyLengthMajor = 0;
			historyLastTimeSaveMajor = -INFINITY;
		}
		else
		{
			historyIndexMinor = 0;
			historyLengthMinor = 0;
			historyLastTimeSaveMinor = -INFINITY;
		}

		// Read first line, determining if data point or metadata.
		reading = oapiReadScenario_nextline(fileToLoad, line);
		if (strlen(line) < 5) // line contains information about planets.
		{
			planetLines = atoi(line);
		}
		else // normal point, so read.
		{
			reading = ReadDataLineToHistory(line);
		}
		lineNr++;

		for (int i = 0; i < planetLines; i++) // skip next lines, as they only describe planets.
		{
			reading = oapiReadScenario_nextline(fileToLoad, line);
			lineNr++;
		}

		// Now come the actual data points, if we haven't started already.
		while (reading)
		{
			reading = oapiReadScenario_nextline(fileToLoad, line);
			lineNr++;

			reading = ReadDataLineToHistory(line);
		}

		// I have done something strange, by saying the index is -1 of the length, so fix here.
		if (showMajor)
		{
			if (historyLengthMajor != 0) historyIndexMajor = (historyIndexMajor - 1 + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;
		}
		else
		{
			if (historyLengthMinor != 0) historyIndexMinor = (historyIndexMinor - 1 + TOTAL_DATA_POINTS) % TOTAL_DATA_POINTS;
		}
	}
}

void TravelMap::FetchFileNames(char* refName)
{
	char folderPath[20];
	sprintf(folderPath, "Images\\Travelmap");
	int savesInFolder = 0;

	for (const auto& entry : fs::directory_iterator(folderPath))
	{
		std::string myString{ entry.path().u8string() };
		char thisFileName[200];
		sprintf(thisFileName, myString.c_str());

		// Check if .txt file.
		if (!_strnicmp(thisFileName + strlen(thisFileName) - 4, ".txt", 4) && savesInFolder < TOTAL_LOAD_FILES)
		{
			// Check if file has same reference as us. Name of file is e.g. 201121_180736_Earth_2.477e+05km.txt, so reference is between 2nd and 3rd underscore.
			int refNameStart = 14; // yymmdd_hhmmss_ is 14 characters
			char* nameSubstr = thisFileName + 16 + refNameStart + 1;
			char *thirdUnderscore = strchr(nameSubstr, '_');
			int fileRefLength = thirdUnderscore - nameSubstr;
			if (!_strnicmp(thisFileName + 16 + refNameStart + 1, refName, fileRefLength)) // 16 is folder path length
			{
				sprintf(availableSaves[savesInFolder], thisFileName);
				savesInFolder++;
			}
		}
	}

	if (savesInFolder == 0)
	{
		sprintf(availableSaves[0], "[empty]");
		totalLoadChoices = 1; // can't be zero.
	}
	else
	{
		totalLoadChoices = savesInFolder;
	}
}

bool TravelMap::ReadDataLineToHistory(char* line, bool flip, char separator, TARGETOBJ saveTo)
{
	bool reading = true;

	char* commaPos1 = strchr(line, separator);

	if (commaPos1 == NULL) reading = false; // empty line, which can only mean end of file.
	else
	{
		char* commaPos2 = strchr(commaPos1 + 1, separator);

		if (commaPos2 != NULL) // we have three good coordinate values.
		{
			double xPos, yPos, zPos;
			xPos = atof(line);
			yPos = atof(commaPos1 + 1);
			zPos = atof(commaPos2 + 1);

			if (flip) // remember, Orbiter uses wrong reference system, so assign y to z, and opposite.
			{
				double buf = yPos;
				yPos = zPos;
				zPos = buf;
			}


			if (saveTo == SAVETOMAJOR)
			{
				majorPos[historyIndexMajor] = _V(xPos, yPos, zPos);
				historyIndexMajor = (historyIndexMajor + 1) % TOTAL_DATA_POINTS;
				if (historyLengthMajor < TOTAL_DATA_POINTS) historyLengthMajor++;
			}
			else if (saveTo == SAVETOMINOR)
			{
				minorPos[historyIndexMinor] = _V(xPos, yPos, zPos);
				historyIndexMinor = (historyIndexMinor + 1) % TOTAL_DATA_POINTS;
				if (historyLengthMinor < TOTAL_DATA_POINTS) historyLengthMinor++;
			}
			else
			{
				if (showMajor)
				{
					majorPos[historyIndexMajor] = _V(xPos, yPos, zPos);
					historyIndexMajor = (historyIndexMajor + 1) % TOTAL_DATA_POINTS;
					if (historyLengthMajor < TOTAL_DATA_POINTS) historyLengthMajor++;
				}
				else
				{
					minorPos[historyIndexMinor] = _V(xPos, yPos, zPos);
					historyIndexMinor = (historyIndexMinor + 1) % TOTAL_DATA_POINTS;
					if (historyLengthMinor < TOTAL_DATA_POINTS) historyLengthMinor++;
				}
			}
		}
	}

	return reading;
}

char *TravelMap::SetPlanetBrush(OBJHANDLE obj, oapi::Sketchpad* skp)
{
	char name[20];
	oapiGetObjectName(obj, name, 20);

	for (int i = 0; i < SAVED_COLOURS; i++)
	{
		if (strcmp(name, SAVED_COLOURS_NAMES[i]) == 0)
		{
			skp->SetBrush(planetBrush[i]);
			return GetColourString(SAVED_COLOUR_HEX[i]);
		}
	}

	skp->SetBrush(genericPlanetBrush);
	return GetColourString(genericColour);
}

char* TravelMap::GetColourString(DWORD colour)
{
	// Swap R and B in RGB
	int B = colour % 256;
	int G = ((colour - B) / 256) % 256;
	int R = (colour - B - G * 256) / (256 * 256);
	colour = B * 256 * 256 + G * 256 + R;

	char *RGB = new char[8];
	sprintf(RGB, "#%.6X", colour); // %X itself converts from int to HEX, with .6 to force 6 characters (may be less if low int)

	return RGB;
}

char* TravelMap::FormatNumber(double num)
{
	char cbuf[100];
	if (num < 1e3) sprintf(cbuf, "%.2f", num);
	else if (num < 1e6) sprintf(cbuf, "%.2f thousand", num / 1e3);
	else if (num < 1e9) sprintf(cbuf, "%.2f million", num / 1e6);
	else if (num < 1e12) sprintf(cbuf, "%.2f billion", num / 1e9);
	else if (num < 1e15) sprintf(cbuf, "%.2f trillion", num / 1e12);
	else sprintf(cbuf, "%.2f quadrillion", num / 1e15);

	return cbuf;
}

char* TravelMap::FormatTime(double num)
{
	char cbuf[100];
	if (num < 3600) sprintf(cbuf, "%.0f seconds", num);
	else if (num < 86400) sprintf(cbuf, "%.2f hours", num / 3600.0);
	else if (num < 86400 * 365.25) sprintf(cbuf, "%.2f days", num / 86400.0);
	else if (num < 86400 * 365.25 * 100) sprintf(cbuf, "%.2f years", num / (86400.0 * 365.25));
	else sprintf(cbuf, "A really long time");

	return cbuf;
}

void TravelMap::FixCoordinateSystem(VECTOR3& v)
{
	// Orbiter has a wrong reference system. Swap y and z.
	double buffer = v.z;
	v.z = v.y;
	v.y = buffer;
}

double TravelMap::MnA2TrA(double MnA, double Ecc)
{
	if (Ecc < 1.0)
	{
		double EccAnom = MnA; // initial guess

		const int maxIterationLength = 15;
		double previousResult = EccAnom;
		int i = 0;
		while (i < maxIterationLength)
		{
			EccAnom -= (EccAnom - Ecc * sin(EccAnom) - MnA) / (1.0 - Ecc * cos(EccAnom));
			i++;

			if (abs(EccAnom - previousResult) < 1e-8) break; // Converged

			previousResult = EccAnom;
		}

		double TrA = 2.0 * atan(sqrt((1.0 + Ecc) / (1.0 - Ecc)) * tan(EccAnom / 2.0));

		return TrA;
	}
	else
	{
		// Hyperbolic equations, from https://en.wikipedia.org/wiki/Hyperbolic_trajectory

		double HypAnom = asinh(MnA / 2.0); // initial guess, hyperbolic anomaly, which has an exponential dependency

		const int maxIterationLength = 30;
		double previousResult = HypAnom;
		int i = 0;
		while (i < maxIterationLength)
		{
			HypAnom -= (HypAnom + MnA - Ecc * sinh(HypAnom)) / (1.0 - Ecc * cosh(HypAnom));
			i++;

			if (abs(HypAnom - previousResult) < 1e-8) break; // Converged

			previousResult = HypAnom;
		}

		double TrA = 2.0 * atan(sqrt((Ecc + 1.0) / (Ecc - 1.0)) * tanh(HypAnom / 2.0));

		return TrA;
	}
}

double TravelMap::EccentricAnomaly(double ecc, double TrA)
{
	if (ecc > 1.0) return 2.0 * atanh(sqrt((ecc - 1.0) / (ecc + 1.0)) * tan(TrA / 2.0)); // hyperbolic
	else return 2.0 * atan(sqrt((1.0 - ecc) / (1.0 + ecc)) * tan(TrA / 2.0)); // elliptic
}
