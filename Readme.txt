Readme for FromCAPto3D software

This is PC software to restore the 3D coordinates of objects from their cylindrically averaged Patterson function (CAP).

Contents

in "bin" 

FromCAPto3D.exe: An executable file, which runs on Microsoft Windows PCs (Windows 7 and higher).

FromCAPto3D.vb: A source file of above, written in Visual Basic 2008.

in "CAPs"

Contains examples of cylindrically averaged Patterson function that can be read by the software.
These are simply the list of independent vectors present in objects, placed in the 1st quadrant of the xz plane.

Triangle2CPat.txt: An objects consisting of just 3 points in the 3D space
actinhelixCPat.txt: Assembly of points arranged with a symmetry of F-actin filament (28/13 helix). Contains 6 monomers (points).
actinhelixLCPat.txt: Assembly of points arranged with a symmetry of F-actin filament (28/13 helix). Contains 28 monomers (points).
DNAhelix10CPat.txt: Assembly of points arranged with a symmetry of the double helix of DNA with a length of 10 base pairs.

in "Restored"

Contains output of the software, after reading the CAPs listed above.
They come in 2 formats: xyz format and obj format.
The former can be read by graphic software packages for chemistry. The latter can be read by general 3DCG software packages.

How to run the software

Simply double-click the icon of FromCAPto3D.exe. It prompts to select a CAP file (*CPat.txt). After the 3D structure is solved, the software prompts to enter the name of the output file to be saved.
note: It takes an increasingly longer time for solving a bigger structure. Be patient!

reference
H. Iwamoto, "The 3-D structure of fibrous material is fully restorable from its X-ray diffraction pattern" IUCrJ, in press.