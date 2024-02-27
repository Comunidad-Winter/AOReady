Attribute VB_Name = "GS_Security"
Option Explicit
Dim tStr As String
Dim cliMD5 As String
Dim Ver As String
Dim tName As String

Public Function ProtocoloPrincipal(ByVal UserIndex As Integer, ByVal rdata As String) As Boolean
    ProtocoloPrincipal = False
    If Left$(rdata, 13) = "gIvEmEvAlcOde" Then
       '<<<<<<<<<<< MODULO PRIVADO DE CADA IMPLEMENTACION >>>>>>
       '<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       
       ' [GS] Anti-AoH
       If AntiAOH = True Then
            tStr = Right(rdata, Len(rdata) - InStrRev(rdata, Chr(126)))
            If IsNumeric(tStr) = False Then
                 Call SendData(ToIndex, UserIndex, 0, "ERRCliente incorrecto.")
                 Call CloseSocket(UserIndex)
            ElseIf val(tStr) < 5 Then
                 Call SendData(ToIndex, UserIndex, 0, "ERRCliente invalido.")
                 Call CloseSocket(UserIndex)
            End If
        End If
       ' [/GS]
       
       ' [GS] Keyfalso
       UserList(UserIndex).flags.ValCoDe = CInt(RandomNumber(20000, 32000))
       UserList(UserIndex).RandKey = CLng(RandomNumber(0, 99999))
       UserList(UserIndex).PrevCRC = UserList(UserIndex).RandKey
       UserList(UserIndex).PacketNumber = 100
       ' [/GS]
       ' [GS] Soporte Cliente 0.11.3
        If Cliente0113 = True Then
            Call SendData(ToIndex, UserIndex, 0, "VAL" & UserList(UserIndex).RandKey & "," & UserList(UserIndex).flags.ValCoDe & ",F9C7DA4A4BDB3E9AF05D625E570CAF80A66C")
        Else
            Call SendData(ToIndex, UserIndex, 0, "VAL" & UserList(UserIndex).RandKey & "," & UserList(UserIndex).flags.ValCoDe)
            Call EnviarConfigServer(UserIndex) 'padrinos, creacion pjs,
        End If
       Exit Function
    End If
    ProtocoloPrincipal = True
End Function

Public Function ProtocoloModulo(ByVal UserIndex As Integer, ByVal rdata As String) As String
       '<<<<<<<<<<< MODULO PRIVADO DE CADA IMPLEMENTACION >>>>>>
       'If False Then
       '     Call LogError("CRC error userindex: " & UserIndex & " rdata: " & rdata)
       '     Call CloseSocket(UserIndex, True)
       '     Debug.Print "ERR CRC " & tStr
       'End If
       'saco el firulete del CRC (cada uno debe utilizar su tecnica)
       
       ' [GS] Anti-AoH
       If AntiAOH = True Then
            tStr = Right(rdata, Len(rdata) - InStrRev(rdata, Chr(126)))
            If IsNumeric(tStr) = False Then
                 Call SendData(ToIndex, UserIndex, 0, "ERRCliente incorrecto.")
                 Call CloseSocket(UserIndex)
                 Exit Function
            ElseIf val(tStr) < 5 Then
                 Call SendData(ToIndex, UserIndex, 0, "ERRCliente invalido.")
                 Call CloseSocket(UserIndex)
                 Exit Function
            End If
        End If
       ' [/GS]
    
       ProtocoloModulo = Mid$(rdata, 1, InStrRev(rdata, "~") - 1)
       '<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>
End Function


Public Function ProtocoloInicio(ByVal UserIndex As Integer, ByVal rdata As String) As Boolean
    ProtocoloInicio = False
        Select Case Left$(rdata, 6)
            Case "OLOGIN"
                rdata = Right$(rdata, Len(rdata) - 6)
                cliMD5 = Right$(ReadField(4, rdata, Asc(",")), 16)
                'rdata = Left$(rdata, Len(rdata) - 16)
                If Not MD5ok(cliMD5) Then
                    Call SendData(ToIndex, UserIndex, 0, "ERREl cliente está dañado, por favor descarguelo nuevamente desde www.argentumonline.com.ar")
                    Exit Function
                End If
                Ver = ReadField(3, rdata, 44)
                If VersionOK(Ver) Then
                    tName = ReadField(1, rdata, 44)
                    
                    If Not AsciiValidos(tName) Then
                        Call SendData(ToIndex, UserIndex, 0, "ERRNombre invalido.")
                        Call CloseSocket(UserIndex, True)
                        Exit Function
                    End If
                    
                    If Not PersonajeExiste(tName) Then
                        Call SendData(ToIndex, UserIndex, 0, "ERREl personaje no existe.")
                        Call CloseSocket(UserIndex, True)
                        Exit Function
                    End If

                    If Not BANCheck(tName) Then

                        If (False) Then
                              Call LogHackAttemp("IP:" & UserList(UserIndex).ip & " intento crear un bot.")
                              Call CloseSocket(UserIndex)
                              Exit Function
                        End If

                        UserList(UserIndex).flags.NoActualizado = False
                        'UserList(UserIndex).flags.NoActualizado = Not VersionesActuales(val(ReadField(5, rdata, 44)), val(ReadField(6, rdata, 44)), val(ReadField(7, rdata, 44)), val(ReadField(8, rdata, 44)), val(ReadField(9, rdata, 44)), val(ReadField(10, rdata, 44)), val(ReadField(11, rdata, 44)))
                        'If UserList(UserIndex).flags.NoActualizado Then
                        'ATENCION ACA SE MANEJAN LAS AUTO ACTUALIZACOINES
                        If False Then
                            Call SendData(ToIndex, UserIndex, 0, "ERRExisten actualizaciones pendientes. Ejecute el programa AutoUpdateClient.exe ubicado en la carpeta del AO para actualizar el juego")
                            Call CloseSocket(UserIndex)
                        End If
                        
                        Dim Pass11 As String
                        Pass11 = ReadField(2, rdata, 44)
                        Call ConnectUser(UserIndex, tName, Pass11)
                    Else
                        Call SendData(ToIndex, UserIndex, 0, "ERRSe te ha prohibido la entrada a Argentum debido a tu mal comportamiento. Consulta en aocp.alkon.com.ar/est para ver el motivo de la prohibición.")
                    End If
                Else
                     Call SendData(ToIndex, UserIndex, 0, "ERREsta version del juego es obsoleta, la version correcta es " & ULTIMAVERSION & ". La misma se encuentra disponible en nuestra pagina.")
                     'Call CloseSocket(UserIndex)
                     Exit Function
                End If
                Exit Function
            Case "TIRDAD"
            
                ' [GS] Dados editables
                UserList(UserIndex).Stats.UserAtributos(1) = Int(RandomNumber(Dados(0), Dados(1)))
                UserList(UserIndex).Stats.UserAtributos(2) = Int(RandomNumber(Dados(0), Dados(1)))
                UserList(UserIndex).Stats.UserAtributos(3) = Int(RandomNumber(Dados(0), Dados(1)))
                UserList(UserIndex).Stats.UserAtributos(4) = Int(RandomNumber(Dados(0), Dados(1)))
                UserList(UserIndex).Stats.UserAtributos(5) = Int(RandomNumber(Dados(0), Dados(1)))
                ' [/GS]
                
                'Barrin 3/10/03
                'Cuando se tiran los dados, el servidor manda un 0 o un 1 dependiendo de si usamos o no el sistema de padrinos
                'así, el cliente sabrá si abrir el frmPasswd con textboxes extra para poner el nombre y pass del padrino o no
                Call SendData(ToIndex, UserIndex, 0, "DADOS" & UserList(UserIndex).Stats.UserAtributos(1) & "," & UserList(UserIndex).Stats.UserAtributos(2) & "," & UserList(UserIndex).Stats.UserAtributos(3) & "," & UserList(UserIndex).Stats.UserAtributos(4) & "," & UserList(UserIndex).Stats.UserAtributos(5) & "," & UsandoSistemaPadrinos)
                
                Exit Function

            Case "NLOGIN"
                
                If PuedeCrearPersonajes = 0 Then
                    Call SendData(ToIndex, UserIndex, 0, "ERRLa creacion de personajes en este servidor se ha deshabilitado.")
                    Call CloseSocket(UserIndex)
                    Exit Function
                End If
                
                If ServerSoloGMs > 0 Then
                    Call SendData(ToIndex, UserIndex, 0, "ERRServidor restringido a administradores. Consulte la página oficial o el foro oficial para mas información.")
                    Call CloseSocket(UserIndex)
                    Exit Function
                End If

                If aClon.MaxPersonajes(UserList(UserIndex).ip) Then
                    Call SendData(ToIndex, UserIndex, 0, "ERRHas creado demasiados personajes.")
                    Call CloseSocket(UserIndex)
                    Exit Function
                End If
                                
                rdata = Right$(rdata, Len(rdata) - 6)
                cliMD5 = Right$(rdata, 16)
                rdata = Left$(rdata, Len(rdata) - 16)
                
                If Not MD5ok(cliMD5) Then
                    Call SendData(ToIndex, UserIndex, 0, "ERREl cliente está dañado o es antiguo, por favor descárguelo nuevamente desde el sitio http://ao.alkon.com.ar")
                    Exit Function
                End If

'                If Not ValidInputNP(rdata) Then Exit Sub
                
                Ver = ReadField(5, rdata, 44)
                If VersionOK(Ver) Then
                     Dim miinteger As Integer
                     If UsandoSistemaPadrinos = 1 Then
                        miinteger = CInt(val(ReadField(46, rdata, 44)))
                     Else
                        miinteger = CInt(val(ReadField(44, rdata, 44)))
                     End If
                        
                     'validacion sobre loginmessage y valcode (privada!)
                     If False Then
                         Call SendData(ToIndex, UserIndex, 0, "ERRPara poder continuar con la creación del personaje, debe utilizar el cliente proporcionado en ao.alkon.com.ar")
                         'Call LogHackAttemp("IP:" & UserList(UserIndex).ip & " intento crear un bot.")
                         Call CloseSocket(UserIndex)
                         Exit Function
                     End If
                     
                     ' [GS] Anti-Nick invalidos
                     If ReadField(1, rdata, 44) = "" Or Left(ReadField(1, rdata, 44), 1) = " " Or Right(ReadField(1, rdata, 44), 1) = " " Then
                         Call SendData(ToIndex, UserIndex, 0, "ERREl nick es invalido.")
                         Call CloseSocket(UserIndex)
                         Exit Function
                     End If
                     ' [/GS]
                     
                     'Barrin 3/10/03
                     'A partir de si usamos el sistema o no, tratamos de conectar al nuevo pjta
                     If UsandoSistemaPadrinos = 1 Then
                        Call ConnectNewUser(UserIndex, ReadField(1, rdata, 44), ReadField(2, rdata, 44), val(ReadField(3, rdata, 44)), ReadField(4, rdata, 44), ReadField(6, rdata, 44), ReadField(7, rdata, 44), _
                        ReadField(8, rdata, 44), ReadField(9, rdata, 44), ReadField(10, rdata, 44), ReadField(11, rdata, 44), ReadField(12, rdata, 44), ReadField(13, rdata, 44), _
                        ReadField(14, rdata, 44), ReadField(15, rdata, 44), ReadField(16, rdata, 44), ReadField(17, rdata, 44), ReadField(18, rdata, 44), ReadField(19, rdata, 44), _
                        ReadField(20, rdata, 44), ReadField(21, rdata, 44), ReadField(22, rdata, 44), ReadField(23, rdata, 44), ReadField(24, rdata, 44), ReadField(25, rdata, 44), _
                        ReadField(26, rdata, 44), ReadField(27, rdata, 44), ReadField(28, rdata, 44), ReadField(29, rdata, 44), ReadField(30, rdata, 44), ReadField(31, rdata, 44), _
                        ReadField(32, rdata, 44), ReadField(33, rdata, 44), ReadField(34, rdata, 44), ReadField(35, rdata, 44), ReadField(36, rdata, 44), ReadField(37, rdata, 44), ReadField(38, rdata, 44))
                     Else
                        UserList(UserIndex).flags.NoActualizado = Not VersionesActuales(val(ReadField(37, rdata, 44)), val(ReadField(38, rdata, 44)), val(ReadField(39, rdata, 44)), val(ReadField(40, rdata, 44)), val(ReadField(41, rdata, 44)), val(ReadField(42, rdata, 44)), val(ReadField(43, rdata, 44)))
                        
                        Call ConnectNewUser(UserIndex, ReadField(1, rdata, 44), ReadField(2, rdata, 44), val(ReadField(3, rdata, 44)), ReadField(4, rdata, 44), ReadField(6, rdata, 44), ReadField(7, rdata, 44), _
                        ReadField(8, rdata, 44), ReadField(9, rdata, 44), ReadField(10, rdata, 44), ReadField(11, rdata, 44), ReadField(12, rdata, 44), ReadField(13, rdata, 44), _
                        ReadField(14, rdata, 44), ReadField(15, rdata, 44), ReadField(16, rdata, 44), ReadField(17, rdata, 44), ReadField(18, rdata, 44), ReadField(19, rdata, 44), _
                        ReadField(20, rdata, 44), ReadField(21, rdata, 44), ReadField(22, rdata, 44), ReadField(23, rdata, 44), ReadField(24, rdata, 44), ReadField(25, rdata, 44), _
                        ReadField(26, rdata, 44), ReadField(27, rdata, 44), ReadField(28, rdata, 44), ReadField(29, rdata, 44), ReadField(30, rdata, 44), ReadField(31, rdata, 44), _
                        ReadField(32, rdata, 44), ReadField(33, rdata, 44), ReadField(34, rdata, 44), ReadField(35, rdata, 44), ReadField(36, rdata, 44))
                     End If
                
                Else
                     Call SendData(ToIndex, UserIndex, 0, "!!Esta version del juego es obsoleta, la version correcta es " & ULTIMAVERSION & ". La misma se encuentra disponible en nuestra pagina.")
                     Exit Function
                End If
                Exit Function
        End Select
    ProtocoloInicio = True
End Function
