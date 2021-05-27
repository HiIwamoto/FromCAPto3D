Option Strict Off
Option Explicit On
Imports VB = Microsoft.VisualBasic

Friend Class Form1

    Inherits System.Windows.Forms.Form

    Public px(1000), py(1000), veclen(1000), np, histo(100), maxhisto, nhisto, histotop10, maxxexp As Single
    Public vecx(1000), vecz(1000), used(1000), nvec, nvertex, vertex(1000), vertey(1000), vertez(1000) As Single
    Public vec2x(1000), vec2z(1000), nvec2, vec2len(1000), nfailure, accuracy, longestvector, permissible_error, unused, unused_present As Single
    Public ij, oierr, yy3, yy2, yy1, yy4, yy5, yy9, xx, Y5, Y6, Y9, x1, x2, initialscreen As Single
    Public colo, ierr, i, k, k2, k3, yy, C, cB As Single
    Public MM, j, ii, TT, t, N, Y4, Y3, Y2, M, B, a, a1, a2, r, Y1, Y, x, z, co, nrep, nrepfinal, success, successiii As Single
    Public q, inid, fbmp, fimage, freference, expfile, maskfile As String
    Public ochiai, zurashita, dire, ophaseerr, PHAS2, PHAS0, phaseerr, phaseerr2, phaseerr3, core, PHAS, ix, iy, ix2, iy2 As Single
    Public iii, iiii, iiiii, ii2, ii3, bestiiii, testiii, minerr2, imanoerr, repea, KISU, jisu, bingo, limx2, limx1, pmax, ntrial, minerr3, use_ref, use_img As Single
    Public Const pai As Single = 3.141593
    Public gr, grn, gro As Graphics
    Public img2, img3 As Image

    Public imgwidth, imgheigh As Single


    Const pi As Double = 3.1415926536




    Public Sub Form1_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load

        'Restoring 3D structure of sample from cylindrically averaged Patterson function (CAP)




        Me.Left = 0
        Me.Top = 0
        Me.Visible = True
        Picture1.Visible = True
        Picture1.Left = 0
        Picture1.Top = 0
        Picture1.Width = 1028
        Picture1.Height = 516

        ListBox1.Top = 520
        ListBox1.Left = 0
        ListBox1.Width = 1024
        ListBox1.Height = 400



        inid = "H:\CDI\512"
        Show()

        Randomize(VB.Timer)

        CD1Open.Filter = "Cylindrically averaged Patterson vector list|*CPat.txt"
        CD1Open.Title = "Open Patterson vector list"
        CD1Open.FileName = ""
        CD1Open.ShowDialog()
        expfile = CD1Open.FileName

        FileOpen(1, expfile, OpenMode.Input)
        N = 0 : longestvector = 0
rep2:   If EOF(1) Then GoTo rep3
        N = N + 1
        Input(1, vec2x(N))  'Reading cylindrically averaged vectors (in a xz plane)
        Input(1, vec2z(N))
        vec2len(N) = Math.Sqrt(vec2x(N) ^ 2 + vec2z(N) ^ 2)  'vector length
        If vec2len(N) > longestvector Then longestvector = vec2len(N) 'the longest vector
        GoTo rep2
rep3:   FileClose(1)
        nvec2 = N


        accuracy = 0.01  'accuracy of fitting
        q = InputBox("accuracy? (accuracy x longest vector is permissible error", , Str(accuracy))
        accuracy = Val(q)

        permissible_error = longestvector * accuracy 'If position error is within this value, the vertex is accepted.
        q = "expfile = " + expfile
        ListBox1.Items.Add(q)
        q = "fitting accuracy = " + Str(accuracy)
        ListBox1.Items.Add(q)
        '

        'Predicting the number of vertices from the number of vectors

        nvertex = (1 + Math.Sqrt(1 + 8 * nvec2)) / 2
        If nvertex - Int(nvertex) <> 0 Then
            q = MsgBox("Wrong vector number", MsgBoxStyle.Exclamation)

        End If
        q = MsgBox("Number of expected vertices =" + Str(nvertex))


        'Sorting vectors and exclude overlapping vectors 
        Dim n2, icchi As Short
        For i = 1 To nvec2
            If i = 1 Then vecx(i) = vec2x(i) : vecz(i) = vec2z(i) : nvec = 1
            If i > 1 Then
                n2 = nvec : icchi = 0
                For j = 1 To n2
                    If vec2x(i) = vecx(j) And vec2z(i) = vecz(j) Then icchi = 1
                Next
                If icchi = 1 Then GoTo skip1
                nvec = nvec + 1
                vecx(nvec) = vec2x(i) : vecz(nvec) = vec2z(i)
            End If
skip1:
        Next i
        q = "number of vectors = " + Str(nvec)
        ListBox1.Items.Add(q)

        drawimg()

        q = MsgBox("Ok?")
        nfailure = 0 'number of failure
        unused = 0 ' number of vectors left unused
start0:

        For i = 1 To nvec
            used(i) = 0
        Next
        unused_present = 0

        'Determining the xyz coordinates of vertices

        vertex(1) = 0 : vertey(1) = 0 : vertez(1) = 0  'The 1st vertex is placed at the origin                            
        q = "vertex 1 = (" + Str(vertex(iiii)) + Str(vertey(iiii)) + Str(vertez(iiii)) + ")"
        ListBox1.Items.Add(q)

        If nfailure = 0 Or unused = 0 Then vertex(2) = vecx(1) : vertey(2) = 0 : vertez(2) = vecz(1) : used(1) = 1 'The 2nd vertex is the end of the 1st vector, on the xz plane.


        If nfailure > 0 And unused > 0 Then vertex(2) = vecx(unused) : vertey(2) = 0 : vertez(2) = vecz(unused) : used(unused) = 1 'If an unused vector is present, restart calculation from the beginning by using that vector as the 1st vector.
        q = "vertex 2 = (" + Str(vertex(2)) + Str(vertey(2)) + Str(vertez(2)) + ")"
        q = "vertex 2 = (" + Str(vertex(2)) + Str(vertey(2)) + Str(vertez(2)) + ")"
        ListBox1.Items.Add(q)
        ListBox1.Refresh()

        'Starting form these 2 vertices, determine the coordinates of the rest of vertices.

        Dim found, ang, newvecx, newvecz, nsuccess, trytofind As Single


        found = 2 'number of found vertices
        iiii = 2
        nfailure = 0
start1:

        iiii = iiii + 1
start2:

        ' Connect the 2nd vector (and later) to already deterimed vertices
        ' This results in the generation of a new vector, and it must coincide with one of the vectors listed in the *CPat.txt
        ' To check this, the new vector is rotated 360 degrees around its starting point, and test is there is a certain angle with which the new vector coincides with the one in the list.
        ' In such an angle is found, its tip is regarded as a correct vertex.
        ' zero-length vectors are excluded from the process because they are always correct.

        For iii = 1 To iiii - 1 ' index of the already determined vertex from which a new vector is attached

            drawimg2()

            For ii2 = 1 To nvec 'index of vector that is newly attached 
                For ii = 1 To -1 Step -2 'polarity along z axis. Both upward and downward directions are tested

                    vertez(iiii) = vertez(iii) + vecz(ii2) * ii
                    newvecz = vecz(ii2) * ii

                    For ii3 = 2 To 1 Step -1
                        For j = 1 To iiii - 1  'index of vertices that are already determined
                            If j = iii Then GoTo skip12

                            For k = 1 To nvec
                                trytofind = 0
                                If Math.Abs(newvecz - vecz(k)) <= permissible_error Then  'z coordinates must coincide
                                    trytofind = 1
                                    If (vertex(iii) - vertex(j)) ^ 2 + (vertey(iii) - vertey(j)) ^ 2 <= (vecx(ii2) + vecx(k)) ^ 2 Then  ' The two circles must cross in order for the two vectors are to be connected

                                        trytofind = 2
                                        Dim c1x, c2x, c1y, c2y As Single  'The coordinates of the centers of the two circles
                                        Dim l As Single 'Distance between the two centers
                                        Dim theta As Single 'Angle between the line connecting the two centers and the x-axis (radian)
                                        Dim theta1 As Single 'Angle between the line connecting the two centers and the line connecting the center and the crossing point for circle 1 (radian)
                                        Dim a1, a2 As Single 'Distance between the center of each circle and the crossing point for the line connecting the two centers and the line connecting the two crossing points of the two circles. a1 + a2 = l
                                        Dim r1, r2 As Single 'radii of two circles
                                        c1x = vertex(iii) : c1y = vertey(iii) : c2x = vertex(j) : c2y = vertey(j) : r1 = vecx(ii2) : r2 = vecx(k)
                                        'determine the crossing points of the two circles

                                        l = Math.Sqrt((c2x - c1x) ^ 2 + (c2y - c1y) ^ 2)

                                        a1 = (l ^ 2 + r1 ^ 2 - r2 ^ 2) / (2 * l)
                                        a2 = l - a1

                                        If r1 <> 0 Then theta1 = Math.Acos(a1 / r1)
                                        If r1 = 0 Then theta1 = pai / 2

                                        If c1x = c2x And c2y > c1y Then theta = pai / 2
                                        If c1x = c2x And c2y < c1y Then theta = pai / -2
                                        If c1x <> c2x Then
                                            If c2x > c1x Then
                                                theta = Math.Atan((c2y - c1y) / (c2x - c1x))
                                            Else
                                                theta = pai + Math.Atan((c2y - c1y) / (c2x - c1x))
                                            End If
                                        End If
                                        ' ang = theta + theta1
                                        ' ang = theta - theta1
                                        'These two possiblities exist

                                        If ii3 = 1 Then ang = theta + theta1
                                        If ii3 = 2 Then ang = theta - theta1


                                        vertex(iiii) = vertex(iii) + vecx(ii2) * Math.Cos(ang)
                                        vertey(iiii) = vertey(iii) + vecx(ii2) * Math.Sin(ang)
                                        If vertex(iiii) = Double.NaN Then Stop


                                        For k3 = 1 To iiii - 1
                                            If Math.Sqrt((vertex(iiii) - vertex(k3)) ^ 2 + (vertey(iiii) - vertey(k3)) ^ 2 + (vertez(iiii) - vertez(k3)) ^ 2) = 0 Then
                                                GoTo skip14 'Do not place a new vortex where there is already a vortex
                                            End If
                                        Next k3


skip17:



                                    End If
                                End If
                                If trytofind < 2 Then GoTo skip19
                                a = k

                                nsuccess = 0
                                For k3 = 1 To iiii - 1  'index for vertices that are already present

                                    x = vertex(iiii) - vertex(k3)
                                    Y = vertey(iiii) - vertey(k3)
                                    newvecx = Math.Sqrt(x ^ 2 + Y ^ 2)
                                    newvecz = Math.Abs(vertez(iiii) - vertez(k3))
                                    success = 0

                                    For k2 = 1 To nvec
                                        If Math.Abs(newvecx - vecx(k2)) <= permissible_error And Math.Abs(newvecz - vecz(k2)) <= permissible_error Then
                                            success = 1
                                        End If
                                    Next k2
                                    success = success * success 'If all the new vectors coincide any of the vectors in the *CPat.txt , it is regarded to be a success (success = 1)
                                    'otherwise success = 0


                                    If success = 1 Then nsuccess = nsuccess + 1

                                Next k3
skip18:
                                If nsuccess = iiii - 1 Then
                                    found = found + 1
                                    Me.Text = Str(found) + " vertices found"
                                    used(ii2) = 1
                                    q = "vertex" + Str(found) + " x=" + Str(newvecx) + ", z=" + Str(newvecz) + ", New vertex=(" + Str(vertex(iiii)) + Str(vertey(iiii)) + Str(vertez(iiii)) + "), ang=" + Str(ang / pai * 180)
                                    ListBox1.Items.Add(q)
                                    ListBox1.Refresh()

                                    GoTo skip11

                                End If
skip19:
                            Next k
skip12:

                        Next j


skip14:
                        a = a
                    Next ii3
                Next ii
            Next ii2
        Next iii
        nfailure = nfailure + 1
        If nfailure = 10 Then
            GoTo finish  'if the number of unsuccessful trials reaches 10, finish calculation
        End If

        'Verify that the recovered 3D structure contains all of the vectors in the list
        Dim checkx, checkz As Single
        For i = 1 To iiii - 1
            For j = 1 To iiii - 1
                If i = j Then GoTo skip15
                checkx = Math.Sqrt((vertex(i) - vertex(j)) ^ 2 + (vertey(i) - vertey(j)) ^ 2)
                checkz = Math.Abs(vertez(i) - vertez(j))
                For k = 1 To nvec
                    If Math.Abs(checkx - vecx(k)) < permissible_error And Math.Abs(checkz - vecz(k)) < permissible_error Then used(k) = 1
                Next
skip15:
            Next
        Next


        For i = 1 To nvec
            If used(i) = 0 Then
                unused = i : unused_present = 1
                q = "vector" + Str(i) + " not used"
                ListBox1.Items.Add(q)
                ListBox1.Refresh()


            End If
        Next i

        If unused_present = 1 Then
            GoTo start0
            q = "Restart from the beginning with unused vector"
            ListBox1.Items.Add(q)
            ListBox1.Refresh()

        Else
            GoTo start2 'If solution is not found, return to the beginning without increasing iiii
        End If

skip11:
        a = a
        If found >= nvertex Then GoTo finish
        GoTo start1
finish:


        q = MsgBox(Str(found) + "/" + Str(nvertex) + " vertices found", MsgBoxStyle.Information)
        q = Str(found) + "/" + Str(nvertex) + " vertices found. Finishing calculation"
        ListBox1.Items.Add(q)



        Dim expfile2 As String

        CD1Save.FileName = VB.Left(expfile, Len(expfile) - 4) + "_Restored.xyz"
        CD1Save.Filter = "XYZ coordinate files|*.xyz"
        CD1Save.Title = "Saving restored structure"
        If CD1Save.ShowDialog() = DialogResult.Cancel Then End

        expfile = CD1Save.FileName
        expfile2 = VB.Left(expfile, Len(expfile) - 4) + ".obj"


        FileOpen(1, expfile, OpenMode.Output)
        FileOpen(2, expfile2, OpenMode.Output)
        Print(1, nvertex, vbCrLf)
        Print(1, "Restored vertices", vbCrLf)
        Print(2, "o " + expfile2, vbCrLf)
        Print(2, "g default", vbCrLf)

        For i = 1 To found
            If i > 1 And vertex(i) = 0 And vertey(i) = 0 And vertez(i) = 0 Then GoTo skip13
            Print(1, "1 ", vertex(i), vertey(i), vertez(i), vbCrLf)
            Print(2, "v ", vertex(i), vertey(i), vertez(i), vbCrLf)
skip13:
        Next
        FileClose(1)
        FileClose(2)

        End








    End Sub

   
    Public Sub drawimg()

        Dim nbitmap As New Bitmap(Picture1.Width + 5, Picture1.Height + 5)
        gr = Graphics.FromImage(nbitmap)
        gr.Clear(Color.Black)

        Dim pen1 As New Pen(Color.FromArgb(255, 255, 0), 1)
        gr.DrawLine(pen1, 0, 256, 512, 256)
        gr.DrawLine(pen1, 256, 0, 256, 512)

        For i = 1 To nvec
            x = 256 + vecx(i)
            Y = 256 - vecz(i)
            gr.FillEllipse(Brushes.White, x - 2, Y - 2, 4, 4)
        Next


        Picture1.Image = nbitmap
        Picture1.Refresh()


    End Sub

    Public Sub drawimg2()

        Dim nbitmap As New Bitmap(Picture1.Width + 5, Picture1.Height + 5)
        Dim r, h As Single
        gr = Graphics.FromImage(nbitmap)
        gr.Clear(Color.Black)

        Dim pen1 As New Pen(Color.FromArgb(255, 255, 0), 1)
        Dim pen2 As New Pen(Color.FromArgb(128, 128, 128), 1)
        Dim pen3 As New Pen(Color.FromArgb(0, 255, 0), 1)
        gr.DrawLine(pen1, 0, 256, 1028, 256)
        gr.DrawLine(pen1, 256, 0, 256, 512)
        gr.DrawLine(pen1, 512, 0, 512, 512)
        gr.DrawLine(pen1, 768, 0, 768, 512)

        For i = 1 To iiii - 1
            x = 256 + vertex(i)
            Y = 256 - vertey(i)
            gr.FillEllipse(Brushes.White, x - 3, Y - 3, 6, 6)
            For j = 1 To nvec
                r = vecx(j)
                gr.DrawEllipse(pen2, x - r, Y - r, r * 2, r * 2)

            Next
        Next

        x = 256 + vertex(iii)
        Y = 256 - vertey(iii)
        gr.FillEllipse(Brushes.Magenta, x - 2, Y - 2, 4, 4)
        For j = 1 To nvec
            r = vecx(j)
            gr.DrawEllipse(pen3, x - r, Y - r, r * 2, r * 2)

        Next

        For i = 1 To iiii - 1
            x = 768 + vertex(i)
            Y = 256 - vertez(i)
            gr.FillEllipse(Brushes.White, x - 3, Y - 3, 6, 6)
            For j = 1 To nvec
                r = vecx(j)
                h = vecz(j)
                gr.DrawLine(pen2, x - r, Y - h, x + r, Y - h)
                gr.DrawLine(pen2, x - r, Y + h, x + r, Y + h)
                gr.DrawLine(pen2, x - r, Y - h, x + r, Y + h)
                gr.DrawLine(pen2, x - r, Y + h, x + r, Y - h)

            Next
        Next

        x = 768 + vertex(iii)
        Y = 256 - vertez(iii)
        gr.FillEllipse(Brushes.Magenta, x - 2, Y - 2, 4, 4)
        For j = 1 To nvec
            r = vecx(j)
            h = vecz(j)
            gr.DrawLine(pen3, x - r, Y - h, x + r, Y - h)
            gr.DrawLine(pen3, x - r, Y + h, x + r, Y + h)
            gr.DrawLine(pen3, x - r, Y - h, x + r, Y + h)
            gr.DrawLine(pen3, x - r, Y + h, x + r, Y - h)

        Next


        Picture1.Image = nbitmap
        Picture1.Refresh()
        System.Windows.Forms.Application.DoEvents()
        nbitmap.Dispose()
        gr.Dispose()

    End Sub


End Class
