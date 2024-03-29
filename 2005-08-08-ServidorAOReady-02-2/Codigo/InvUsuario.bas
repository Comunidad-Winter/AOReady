Attribute VB_Name = "InvUsuario"
'Argentum Online 0.11.20
'Copyright (C) 2002 M�rquez Pablo Ignacio
'
'This program is free software; you can redistribute it and/or modify
'it under the terms of the GNU General Public License as published by
'the Free Software Foundation; either version 2 of the License, or
'any later version.
'
'This program is distributed in the hope that it will be useful,
'but WITHOUT ANY WARRANTY; without even the implied warranty of
'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'GNU General Public License for more details.
'
'You should have received a copy of the GNU General Public License
'along with this program; if not, write to the Free Software
'Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'
'Argentum Online is based on Baronsoft's VB6 Online RPG
'You can contact the original creator of ORE at aaron@baronsoft.com
'for more information about ORE please visit http://www.baronsoft.com/
'
'
'You can contact me at:
'morgolock@speedy.com.ar
'www.geocities.com/gmorgolock
'Calle 3 n�mero 983 piso 7 dto A
'La Plata - Pcia, Buenos Aires - Republica Argentina
'C�digo Postal 1900
'Pablo Ignacio M�rquez


Option Explicit

Public Function TieneObjetosRobables(ByVal UserIndex As Integer) As Boolean

'17/09/02
'Agregue que la funci�n se asegure que el objeto no es un barco

On Error Resume Next

Dim i As Integer
Dim ObjIndex As Integer

For i = 1 To MAX_INVENTORY_SLOTS
    ObjIndex = UserList(UserIndex).Invent.Object(i).ObjIndex
    If ObjIndex > 0 Then
            If (ObjData(ObjIndex).ObjType <> OBJTYPE_LLAVES And _
                ObjData(ObjIndex).ObjType <> OBJTYPE_BARCOS) Then
                  TieneObjetosRobables = True
                  Exit Function
            End If
    
    End If
Next i


End Function

Function ClasePuedeUsarItem(ByVal UserIndex As Integer, ByVal ObjIndex As Integer) As Boolean
On Error GoTo manejador

'Call LogTarea("ClasePuedeUsarItem")

Dim flag As Boolean

If ObjData(ObjIndex).ClaseProhibida(1) <> "" Then
    
    Dim i As Integer
    For i = 1 To NUMCLASES
        If ObjData(ObjIndex).ClaseProhibida(i) = UCase$(UserList(UserIndex).Clase) Then
                ClasePuedeUsarItem = False
                Exit Function
        End If
    Next i
    
Else
    
    

End If

ClasePuedeUsarItem = True

Exit Function

manejador:
    LogError ("Error en ClasePuedeUsarItem")
End Function

Sub QuitarNewbieObj(ByVal UserIndex As Integer)
Dim j As Integer
For j = 1 To MAX_INVENTORY_SLOTS
        If UserList(UserIndex).Invent.Object(j).ObjIndex > 0 Then
             
             If ObjData(UserList(UserIndex).Invent.Object(j).ObjIndex).Newbie = 1 Then _
                    Call QuitarUserInvItem(UserIndex, j, MAX_INVENTORY_OBJS)
                    Call UpdateUserInv(False, UserIndex, j)
        
        End If
Next

'[Barrin 17-12-03] Si el usuario dej� de ser Newbie, y estaba en el Newbie Dungeon
'es transportado a su hogar de origen ;)
If UserList(UserIndex).Pos.Map = 37 Then
    
    Dim DeDonde As WorldPos
    
    Select Case UCase$(UserList(UserIndex).Hogar)
        Case "LINDOS" 'Vamos a tener que ir por todo el desierto... uff!
            DeDonde = Lindos
        Case "ULLATHORPE"
            DeDonde = Ullathorpe
        Case "BANDERBILL"
            DeDonde = Banderbill
        Case Else
            DeDonde = Nix
    End Select
       
    Call WarpUserChar(UserIndex, DeDonde.Map, DeDonde.X, DeDonde.Y, True)

End If
'[/Barrin]

End Sub

Sub LimpiarInventario(ByVal UserIndex As Integer)


Dim j As Integer
For j = 1 To MAX_INVENTORY_SLOTS
        UserList(UserIndex).Invent.Object(j).ObjIndex = 0
        UserList(UserIndex).Invent.Object(j).Amount = 0
        UserList(UserIndex).Invent.Object(j).Equipped = 0
        
Next

UserList(UserIndex).Invent.NroItems = 0

UserList(UserIndex).Invent.ArmourEqpObjIndex = 0
UserList(UserIndex).Invent.ArmourEqpSlot = 0

UserList(UserIndex).Invent.WeaponEqpObjIndex = 0
UserList(UserIndex).Invent.WeaponEqpSlot = 0

UserList(UserIndex).Invent.CascoEqpObjIndex = 0
UserList(UserIndex).Invent.CascoEqpSlot = 0

UserList(UserIndex).Invent.EscudoEqpObjIndex = 0
UserList(UserIndex).Invent.EscudoEqpSlot = 0

UserList(UserIndex).Invent.HerramientaEqpObjIndex = 0
UserList(UserIndex).Invent.HerramientaEqpSlot = 0

UserList(UserIndex).Invent.MunicionEqpObjIndex = 0
UserList(UserIndex).Invent.MunicionEqpSlot = 0

UserList(UserIndex).Invent.BarcoObjIndex = 0
UserList(UserIndex).Invent.BarcoSlot = 0

End Sub

Sub TirarOro(ByVal Cantidad As Long, ByVal UserIndex As Integer)
On Error GoTo errhandler

If Cantidad > 100000 Then Exit Sub

'SI EL NPC TIENE ORO LO TIRAMOS
If (Cantidad > 0) And (Cantidad <= UserList(UserIndex).Stats.GLD) Then
        Dim i As Byte
        Dim MiObj As Obj
        'info debug
        Dim loops As Integer
        
        'Seguridad Alkon
        If Cantidad > 49999 Then
            Dim j As Integer
            Dim k As Integer
            Dim M As Integer
            Dim Cercanos As String
            For j = UserList(UserIndex).Pos.X - 5 To UserList(UserIndex).Pos.X + 5
                For k = UserList(UserIndex).Pos.Y - 5 To UserList(UserIndex).Pos.Y + 5
                    If LegalPos(M, j, k, True) Then
                        If MapData(M, j, k).UserIndex > 0 Then
                            Cercanos = Cercanos & UserList(MapData(M, j, k).UserIndex).Name & ","
                        End If
                    End If
                Next k
            Next j
            Call LogDesarrollo(UserList(UserIndex).Name & " tira oro. Cercanos: " & Cercanos)
        End If
        '/Seguridad
        
        Do While (Cantidad > 0) And (UserList(UserIndex).Stats.GLD > 0)
            
            If Cantidad > MAX_INVENTORY_OBJS And UserList(UserIndex).Stats.GLD > MAX_INVENTORY_OBJS Then
                MiObj.Amount = MAX_INVENTORY_OBJS
                UserList(UserIndex).Stats.GLD = UserList(UserIndex).Stats.GLD - MAX_INVENTORY_OBJS
                Cantidad = Cantidad - MiObj.Amount
            Else
                MiObj.Amount = Cantidad
                UserList(UserIndex).Stats.GLD = UserList(UserIndex).Stats.GLD - Cantidad
                Cantidad = Cantidad - MiObj.Amount
            End If

            MiObj.ObjIndex = iORO
            
            If UserList(UserIndex).flags.Privilegios > 0 Then Call LogGM(UserList(UserIndex).Name, "Tiro cantidad:" & MiObj.Amount & " Objeto:" & ObjData(MiObj.ObjIndex).Name, False)
            
            Call TirarItemAlPiso(UserList(UserIndex).Pos, MiObj)
            
            'info debug
            loops = loops + 1
            If loops > 100 Then
                LogError ("Error en tiraroro")
                Exit Sub
            End If
            
        Loop
    
End If

Exit Sub

errhandler:

End Sub

Sub QuitarUserInvItem(ByVal UserIndex As Integer, ByVal Slot As Byte, ByVal Cantidad As Integer)

Dim MiObj As Obj
'Desequipa
If Slot < 1 Or Slot > MAX_INVENTORY_SLOTS Then Exit Sub

If UserList(UserIndex).Invent.Object(Slot).Equipped = 1 Then Call Desequipar(UserIndex, Slot)

'Quita un objeto
UserList(UserIndex).Invent.Object(Slot).Amount = UserList(UserIndex).Invent.Object(Slot).Amount - Cantidad
'�Quedan mas?
If UserList(UserIndex).Invent.Object(Slot).Amount <= 0 Then
    UserList(UserIndex).Invent.NroItems = UserList(UserIndex).Invent.NroItems - 1
    UserList(UserIndex).Invent.Object(Slot).ObjIndex = 0
    UserList(UserIndex).Invent.Object(Slot).Amount = 0
End If
    
End Sub

Sub UpdateUserInv(ByVal UpdateAll As Boolean, ByVal UserIndex As Integer, ByVal Slot As Byte)

Dim NullObj As UserOBJ
Dim LoopC As Byte

'Actualiza un solo slot
If Not UpdateAll Then

    'Actualiza el inventario
    If UserList(UserIndex).Invent.Object(Slot).ObjIndex > 0 Then
        Call ChangeUserInv(UserIndex, Slot, UserList(UserIndex).Invent.Object(Slot))
    Else
        Call ChangeUserInv(UserIndex, Slot, NullObj)
    End If

Else

'Actualiza todos los slots
    For LoopC = 1 To MAX_INVENTORY_SLOTS

        'Actualiza el inventario
        If UserList(UserIndex).Invent.Object(LoopC).ObjIndex > 0 Then
            Call ChangeUserInv(UserIndex, LoopC, UserList(UserIndex).Invent.Object(LoopC))
        Else
            
            Call ChangeUserInv(UserIndex, LoopC, NullObj)
            
        End If

    Next LoopC

End If

End Sub

Sub DropObj(ByVal UserIndex As Integer, ByVal Slot As Byte, ByVal num As Integer, ByVal Map As Integer, ByVal X As Integer, ByVal Y As Integer)

Dim Obj As Obj

If num > 0 Then
  
  If num > UserList(UserIndex).Invent.Object(Slot).Amount Then num = UserList(UserIndex).Invent.Object(Slot).Amount
  
  'Check objeto en el suelo
  If MapData(UserList(UserIndex).Pos.Map, X, Y).OBJInfo.ObjIndex = 0 Then
        If UserList(UserIndex).Invent.Object(Slot).Equipped = 1 Then Call Desequipar(UserIndex, Slot)
        Obj.ObjIndex = UserList(UserIndex).Invent.Object(Slot).ObjIndex
        
'        If ObjData(Obj.ObjIndex).Newbie = 1 And EsNewbie(UserIndex) Then
'            Call SendData(ToIndex, UserIndex, 0, "||No podes tirar el objeto." & FONTTYPE_INFO)
'            Exit Sub
'        End If
        
        Obj.Amount = num
        
        Call MakeObj(ToMap, 0, Map, Obj, Map, X, Y)
        Call QuitarUserInvItem(UserIndex, Slot, num)
        Call UpdateUserInv(False, UserIndex, Slot)
        
        If ObjData(Obj.ObjIndex).ObjType = OBJTYPE_BARCOS Then
            Call SendData(ToIndex, UserIndex, 0, "||��ATENCION!! �ACABAS DE TIRAR TU BARCA!" & FONTTYPE_TALK)
        End If
        If ObjData(Obj.ObjIndex).Caos = 1 Or ObjData(Obj.ObjIndex).Real = 1 Then
            Call SendData(ToIndex, UserIndex, 0, "||�ATENCION!! ��ACABAS DE TIRAR TU ARMADURA FACCIONARIA!!" & FONTTYPE_TALK)
        End If
        
        If UserList(UserIndex).flags.Privilegios > 0 Then Call LogGM(UserList(UserIndex).Name, "Tiro cantidad:" & num & " Objeto:" & ObjData(Obj.ObjIndex).Name, False)
  Else
    Call SendData(ToIndex, UserIndex, 0, "||No hay espacio en el piso." & FONTTYPE_INFO)
  End If
    
End If

End Sub

Sub EraseObj(ByVal sndRoute As Byte, ByVal sndIndex As Integer, ByVal sndMap As Integer, ByVal num As Integer, ByVal Map As Byte, ByVal X As Integer, ByVal Y As Integer)

MapData(Map, X, Y).OBJInfo.Amount = MapData(Map, X, Y).OBJInfo.Amount - num

If MapData(Map, X, Y).OBJInfo.Amount <= 0 Then
    MapData(Map, X, Y).OBJInfo.ObjIndex = 0
    MapData(Map, X, Y).OBJInfo.Amount = 0
    Call SendData(sndRoute, sndIndex, sndMap, "BO" & X & "," & Y)
End If

End Sub

Sub MakeObj(ByVal sndRoute As Byte, ByVal sndIndex As Integer, ByVal sndMap As Integer, Obj As Obj, Map As Integer, ByVal X As Integer, ByVal Y As Integer)

If Obj.ObjIndex > 0 And Obj.ObjIndex <= UBound(ObjData) Then
'Crea un Objeto
    MapData(Map, X, Y).OBJInfo = Obj
    Call SendData(sndRoute, sndIndex, sndMap, "HO" & ObjData(Obj.ObjIndex).GrhIndex & "," & X & "," & Y)
End If

End Sub

Function MeterItemEnInventario(ByVal UserIndex As Integer, ByRef MiObj As Obj) As Boolean
On Error GoTo errhandler

'Call LogTarea("MeterItemEnInventario")
 
Dim X As Integer
Dim Y As Integer
Dim Slot As Byte

'�el user ya tiene un objeto del mismo tipo?
Slot = 1
Do Until UserList(UserIndex).Invent.Object(Slot).ObjIndex = MiObj.ObjIndex And _
         UserList(UserIndex).Invent.Object(Slot).Amount + MiObj.Amount <= MAX_INVENTORY_OBJS
   Slot = Slot + 1
   If Slot > MAX_INVENTORY_SLOTS Then
         Exit Do
   End If
Loop
    
'Sino busca un slot vacio
If Slot > MAX_INVENTORY_SLOTS Then
   Slot = 1
   Do Until UserList(UserIndex).Invent.Object(Slot).ObjIndex = 0
       Slot = Slot + 1
       If Slot > MAX_INVENTORY_SLOTS Then
           Call SendData(ToIndex, UserIndex, 0, "||No podes cargar mas objetos." & FONTTYPE_FIGHT)
           MeterItemEnInventario = False
           Exit Function
       End If
   Loop
   UserList(UserIndex).Invent.NroItems = UserList(UserIndex).Invent.NroItems + 1
End If
    
'Mete el objeto
If UserList(UserIndex).Invent.Object(Slot).Amount + MiObj.Amount <= MAX_INVENTORY_OBJS Then
   'Menor que MAX_INV_OBJS
   UserList(UserIndex).Invent.Object(Slot).ObjIndex = MiObj.ObjIndex
   UserList(UserIndex).Invent.Object(Slot).Amount = UserList(UserIndex).Invent.Object(Slot).Amount + MiObj.Amount
Else
   UserList(UserIndex).Invent.Object(Slot).Amount = MAX_INVENTORY_OBJS
End If
    
MeterItemEnInventario = True
       
Call UpdateUserInv(False, UserIndex, Slot)


Exit Function
errhandler:

End Function


Sub GetObj(ByVal UserIndex As Integer)

Dim Obj As ObjData
Dim MiObj As Obj

'�Hay algun obj?
If MapData(UserList(UserIndex).Pos.Map, UserList(UserIndex).Pos.X, UserList(UserIndex).Pos.Y).OBJInfo.ObjIndex > 0 Then
    '�Esta permitido agarrar este obj?
    If ObjData(MapData(UserList(UserIndex).Pos.Map, UserList(UserIndex).Pos.X, UserList(UserIndex).Pos.Y).OBJInfo.ObjIndex).Agarrable <> 1 Then
        Dim X As Integer
        Dim Y As Integer
        Dim Slot As Byte
        
        X = UserList(UserIndex).Pos.X
        Y = UserList(UserIndex).Pos.Y
        Obj = ObjData(MapData(UserList(UserIndex).Pos.Map, UserList(UserIndex).Pos.X, UserList(UserIndex).Pos.Y).OBJInfo.ObjIndex)
        MiObj.Amount = MapData(UserList(UserIndex).Pos.Map, X, Y).OBJInfo.Amount
        MiObj.ObjIndex = MapData(UserList(UserIndex).Pos.Map, X, Y).OBJInfo.ObjIndex
        
        ' [GS] Si es ORO, lo autoequipamos
        If MiObj.ObjIndex = 12 Then
            Call EraseObj(ToMap, 0, UserList(UserIndex).Pos.Map, MapData(UserList(UserIndex).Pos.Map, X, Y).OBJInfo.Amount, UserList(UserIndex).Pos.Map, UserList(UserIndex).Pos.X, UserList(UserIndex).Pos.Y)
            UserList(UserIndex).Stats.GLD = UserList(UserIndex).Stats.GLD + MiObj.Amount
            Call SendUserStatsBox(UserIndex)
            Exit Sub
        End If
        ' [/GS]
        
        If Not MeterItemEnInventario(UserIndex, MiObj) Then
            'Call SendData(ToIndex, UserIndex, 0, "||No puedo cargar mas objetos." & FONTTYPE_INFO)   ' [GS] El mensaje es removido
        Else
            'Quitamos el objeto
            Call EraseObj(ToMap, 0, UserList(UserIndex).Pos.Map, MapData(UserList(UserIndex).Pos.Map, X, Y).OBJInfo.Amount, UserList(UserIndex).Pos.Map, UserList(UserIndex).Pos.X, UserList(UserIndex).Pos.Y)
            'If UserList(UserIndex).flags.Privilegios > 0 Then Call LogGM(UserList(UserIndex).Name, "Agarro:" & MiObj.Amount & " Objeto:" & ObjData(MiObj.ObjIndex).Name, False) ' [GS] Removida esta info
        End If
        
    End If
Else
    Call SendData(ToIndex, UserIndex, 0, "||No hay nada aqui." & FONTTYPE_INFO)
End If

End Sub

Sub Desequipar(ByVal UserIndex As Integer, ByVal Slot As Byte)
'Desequipa el item slot del inventario
Dim Obj As ObjData


If (Slot < LBound(UserList(UserIndex).Invent.Object)) Or (Slot > UBound(UserList(UserIndex).Invent.Object)) Then
    Exit Sub
ElseIf UserList(UserIndex).Invent.Object(Slot).ObjIndex = 0 Then
    Exit Sub
End If

Obj = ObjData(UserList(UserIndex).Invent.Object(Slot).ObjIndex)

Select Case Obj.ObjType


    Case OBJTYPE_WEAPON

        UserList(UserIndex).Invent.Object(Slot).Equipped = 0
        UserList(UserIndex).Invent.WeaponEqpObjIndex = 0
        UserList(UserIndex).Invent.WeaponEqpSlot = 0
        If Not UserList(UserIndex).flags.Mimetizado = 1 Then
            UserList(UserIndex).Char.WeaponAnim = NingunArma
            Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
        End If
    Case OBJTYPE_FLECHAS
    
        UserList(UserIndex).Invent.Object(Slot).Equipped = 0
        UserList(UserIndex).Invent.MunicionEqpObjIndex = 0
        UserList(UserIndex).Invent.MunicionEqpSlot = 0
    
    Case OBJTYPE_HERRAMIENTAS
    
        UserList(UserIndex).Invent.Object(Slot).Equipped = 0
        UserList(UserIndex).Invent.HerramientaEqpObjIndex = 0
        UserList(UserIndex).Invent.HerramientaEqpSlot = 0
    
    Case OBJTYPE_ARMOUR
        
        Select Case Obj.SubTipo
        
            Case OBJTYPE_ARMADURA
                UserList(UserIndex).Invent.Object(Slot).Equipped = 0
                UserList(UserIndex).Invent.ArmourEqpObjIndex = 0
                UserList(UserIndex).Invent.ArmourEqpSlot = 0
                Call DarCuerpoDesnudo(UserIndex, UserList(UserIndex).flags.Mimetizado = 1)
                Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
                
            Case OBJTYPE_CASCO
                UserList(UserIndex).Invent.Object(Slot).Equipped = 0
                UserList(UserIndex).Invent.CascoEqpObjIndex = 0
                UserList(UserIndex).Invent.CascoEqpSlot = 0
                If Not UserList(UserIndex).flags.Mimetizado = 1 Then
                    UserList(UserIndex).Char.CascoAnim = NingunCasco
                    Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
                End If
            Case OBJTYPE_ESCUDO
                UserList(UserIndex).Invent.Object(Slot).Equipped = 0
                UserList(UserIndex).Invent.EscudoEqpObjIndex = 0
                UserList(UserIndex).Invent.EscudoEqpSlot = 0
                If Not UserList(UserIndex).flags.Mimetizado = 1 Then
                    UserList(UserIndex).Char.ShieldAnim = NingunEscudo
                    Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
                End If
        End Select
        
    
End Select

Call SendUserStatsBox(UserIndex)
Call UpdateUserInv(False, UserIndex, Slot)

End Sub
Function SexoPuedeUsarItem(ByVal UserIndex As Integer, ByVal ObjIndex As Integer) As Boolean
On Error GoTo errhandler

If ObjData(ObjIndex).Mujer = 1 Then
    SexoPuedeUsarItem = UCase$(UserList(UserIndex).Genero) <> "HOMBRE"
ElseIf ObjData(ObjIndex).Hombre = 1 Then
    SexoPuedeUsarItem = UCase$(UserList(UserIndex).Genero) <> "MUJER"
Else
    SexoPuedeUsarItem = True
End If

Exit Function
errhandler:
    Call LogError("SexoPuedeUsarItem")
End Function


Function FaccionPuedeUsarItem(ByVal UserIndex As Integer, ByVal ObjIndex As Integer) As Boolean

If ObjData(ObjIndex).Real = 1 Then
    If Not Criminal(UserIndex) Then
        FaccionPuedeUsarItem = UserList(UserIndex).Faccion.ArmadaReal = 1
    Else
        FaccionPuedeUsarItem = False
    End If
ElseIf ObjData(ObjIndex).Caos = 1 Then
    If Criminal(UserIndex) Then
        FaccionPuedeUsarItem = UserList(UserIndex).Faccion.FuerzasCaos = 1
    Else
        FaccionPuedeUsarItem = False
    End If
Else
    FaccionPuedeUsarItem = True
End If

End Function

Sub EquiparInvItem(ByVal UserIndex As Integer, ByVal Slot As Byte)
On Error GoTo errhandler

'Equipa un item del inventario
Dim Obj As ObjData
Dim ObjIndex As Integer

ObjIndex = UserList(UserIndex).Invent.Object(Slot).ObjIndex
Obj = ObjData(ObjIndex)

If Obj.Newbie = 1 And Not EsNewbie(UserIndex) Then
     Call SendData(ToIndex, UserIndex, 0, "||Solo los newbies pueden usar este objeto." & FONTTYPE_INFO)
     Exit Sub
End If
        
Select Case Obj.ObjType
    Case OBJTYPE_WEAPON
       If ClasePuedeUsarItem(UserIndex, ObjIndex) And _
          FaccionPuedeUsarItem(UserIndex, ObjIndex) Then
                'Si esta equipado lo quita
                If UserList(UserIndex).Invent.Object(Slot).Equipped Then
                    'Quitamos del inv el item
                    Call Desequipar(UserIndex, Slot)
                    'Animacion por defecto
                    If UserList(UserIndex).flags.Mimetizado = 1 Then
                        UserList(UserIndex).CharMimetizado.WeaponAnim = NingunArma
                    Else
                        UserList(UserIndex).Char.WeaponAnim = NingunArma
                        Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
                    End If
                    Exit Sub
                End If
                
                'Quitamos el elemento anterior
                If UserList(UserIndex).Invent.WeaponEqpObjIndex > 0 Then
                    Call Desequipar(UserIndex, UserList(UserIndex).Invent.WeaponEqpSlot)
                End If
        
                UserList(UserIndex).Invent.Object(Slot).Equipped = 1
                UserList(UserIndex).Invent.WeaponEqpObjIndex = UserList(UserIndex).Invent.Object(Slot).ObjIndex
                UserList(UserIndex).Invent.WeaponEqpSlot = Slot
                
                'Sonido
                Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SOUND_SACARARMA)
        
                If UserList(UserIndex).flags.Mimetizado = 1 Then
                    UserList(UserIndex).CharMimetizado.WeaponAnim = Obj.WeaponAnim
                Else
                    UserList(UserIndex).Char.WeaponAnim = Obj.WeaponAnim
                    Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
                End If
       Else
            Call SendData(ToIndex, UserIndex, 0, "||Tu clase no puede usar este objeto." & FONTTYPE_INFO)
       End If
    Case OBJTYPE_HERRAMIENTAS
       If ClasePuedeUsarItem(UserIndex, ObjIndex) And _
          FaccionPuedeUsarItem(UserIndex, ObjIndex) Then
                'Si esta equipado lo quita
                If UserList(UserIndex).Invent.Object(Slot).Equipped Then
                    'Quitamos del inv el item
                    Call Desequipar(UserIndex, Slot)
                    Exit Sub
                End If
                
                'Quitamos el elemento anterior
                If UserList(UserIndex).Invent.HerramientaEqpObjIndex > 0 Then
                    Call Desequipar(UserIndex, UserList(UserIndex).Invent.HerramientaEqpSlot)
                End If
        
                UserList(UserIndex).Invent.Object(Slot).Equipped = 1
                UserList(UserIndex).Invent.HerramientaEqpObjIndex = ObjIndex
                UserList(UserIndex).Invent.HerramientaEqpSlot = Slot
                
       Else
            Call SendData(ToIndex, UserIndex, 0, "||Tu clase no puede usar este objeto." & FONTTYPE_INFO)
       End If
    Case OBJTYPE_FLECHAS
       If ClasePuedeUsarItem(UserIndex, UserList(UserIndex).Invent.Object(Slot).ObjIndex) And _
          FaccionPuedeUsarItem(UserIndex, UserList(UserIndex).Invent.Object(Slot).ObjIndex) Then
                
                'Si esta equipado lo quita
                If UserList(UserIndex).Invent.Object(Slot).Equipped Then
                    'Quitamos del inv el item
                    Call Desequipar(UserIndex, Slot)
                    Exit Sub
                End If
                
                'Quitamos el elemento anterior
                If UserList(UserIndex).Invent.MunicionEqpObjIndex > 0 Then
                    Call Desequipar(UserIndex, UserList(UserIndex).Invent.MunicionEqpSlot)
                End If
        
                UserList(UserIndex).Invent.Object(Slot).Equipped = 1
                UserList(UserIndex).Invent.MunicionEqpObjIndex = UserList(UserIndex).Invent.Object(Slot).ObjIndex
                UserList(UserIndex).Invent.MunicionEqpSlot = Slot
                
       Else
            Call SendData(ToIndex, UserIndex, 0, "||Tu clase no puede usar este objeto." & FONTTYPE_INFO)
       End If
    
    Case OBJTYPE_ARMOUR
         
         If UserList(UserIndex).flags.Navegando = 1 Then Exit Sub
         
         Select Case Obj.SubTipo
         
            Case OBJTYPE_ARMADURA
                'Nos aseguramos que puede usarla
                If ClasePuedeUsarItem(UserIndex, UserList(UserIndex).Invent.Object(Slot).ObjIndex) And _
                   SexoPuedeUsarItem(UserIndex, UserList(UserIndex).Invent.Object(Slot).ObjIndex) And _
                   CheckRazaUsaRopa(UserIndex, UserList(UserIndex).Invent.Object(Slot).ObjIndex) And _
                   FaccionPuedeUsarItem(UserIndex, UserList(UserIndex).Invent.Object(Slot).ObjIndex) Then
                   
                   'Si esta equipado lo quita
                    If UserList(UserIndex).Invent.Object(Slot).Equipped Then
                        Call Desequipar(UserIndex, Slot)
                        Call DarCuerpoDesnudo(UserIndex, UserList(UserIndex).flags.Mimetizado = 1)
                        If Not UserList(UserIndex).flags.Mimetizado = 1 Then
                            Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
                        End If
                        Exit Sub
                    End If
            
                    'Quita el anterior
                    If UserList(UserIndex).Invent.ArmourEqpObjIndex > 0 Then
                        Call Desequipar(UserIndex, UserList(UserIndex).Invent.ArmourEqpSlot)
                    End If
            
                    'Lo equipa
                    UserList(UserIndex).Invent.Object(Slot).Equipped = 1
                    UserList(UserIndex).Invent.ArmourEqpObjIndex = UserList(UserIndex).Invent.Object(Slot).ObjIndex
                    UserList(UserIndex).Invent.ArmourEqpSlot = Slot
                        
                    If UserList(UserIndex).flags.Mimetizado = 1 Then
                        UserList(UserIndex).CharMimetizado.Body = Obj.Ropaje
                    Else
                        UserList(UserIndex).Char.Body = Obj.Ropaje
                        Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
                    End If
                    UserList(UserIndex).flags.Desnudo = 0
                    

                Else
                    Call SendData(ToIndex, UserIndex, 0, "||Tu clase,genero o raza no puede usar este objeto." & FONTTYPE_INFO)
                End If
            Case OBJTYPE_CASCO
                If ClasePuedeUsarItem(UserIndex, UserList(UserIndex).Invent.Object(Slot).ObjIndex) Then
                    'Si esta equipado lo quita
                    If UserList(UserIndex).Invent.Object(Slot).Equipped Then
                        Call Desequipar(UserIndex, Slot)
                        If UserList(UserIndex).flags.Mimetizado = 1 Then
                            UserList(UserIndex).CharMimetizado.CascoAnim = NingunCasco
                        Else
                            UserList(UserIndex).Char.CascoAnim = NingunCasco
                            Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
                        End If
                        Exit Sub
                    End If
            
                    'Quita el anterior
                    If UserList(UserIndex).Invent.CascoEqpObjIndex > 0 Then
                        Call Desequipar(UserIndex, UserList(UserIndex).Invent.CascoEqpSlot)
                    End If
            
                    'Lo equipa
                    
                    UserList(UserIndex).Invent.Object(Slot).Equipped = 1
                    UserList(UserIndex).Invent.CascoEqpObjIndex = UserList(UserIndex).Invent.Object(Slot).ObjIndex
                    UserList(UserIndex).Invent.CascoEqpSlot = Slot
                    If UserList(UserIndex).flags.Mimetizado = 1 Then
                        UserList(UserIndex).CharMimetizado.CascoAnim = Obj.CascoAnim
                    Else
                        UserList(UserIndex).Char.CascoAnim = Obj.CascoAnim
                        Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
                    End If
                Else
                    Call SendData(ToIndex, UserIndex, 0, "||Tu clase no puede usar este objeto." & FONTTYPE_INFO)
                End If
            Case OBJTYPE_ESCUDO
                If ClasePuedeUsarItem(UserIndex, UserList(UserIndex).Invent.Object(Slot).ObjIndex) And _
                    FaccionPuedeUsarItem(UserIndex, UserList(UserIndex).Invent.Object(Slot).ObjIndex) Then
       
                    'Si esta equipado lo quita
                    If UserList(UserIndex).Invent.Object(Slot).Equipped Then
                        Call Desequipar(UserIndex, Slot)
                        If UserList(UserIndex).flags.Mimetizado = 1 Then
                            UserList(UserIndex).CharMimetizado.ShieldAnim = NingunEscudo
                        Else
                            UserList(UserIndex).Char.ShieldAnim = NingunEscudo
                            Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
                        End If
                        Exit Sub
                    End If
            
                    'Quita el anterior
                    If UserList(UserIndex).Invent.EscudoEqpObjIndex > 0 Then
                        Call Desequipar(UserIndex, UserList(UserIndex).Invent.EscudoEqpSlot)
                    End If
            
                    'Lo equipa
                    
                    UserList(UserIndex).Invent.Object(Slot).Equipped = 1
                    UserList(UserIndex).Invent.EscudoEqpObjIndex = UserList(UserIndex).Invent.Object(Slot).ObjIndex
                    UserList(UserIndex).Invent.EscudoEqpSlot = Slot
                    
                    If UserList(UserIndex).flags.Mimetizado = 1 Then
                        UserList(UserIndex).CharMimetizado.ShieldAnim = Obj.ShieldAnim
                    Else
                        UserList(UserIndex).Char.ShieldAnim = Obj.ShieldAnim
                        
                        Call ChangeUserChar(ToMap, 0, UserList(UserIndex).Pos.Map, UserIndex, UserList(UserIndex).Char.Body, UserList(UserIndex).Char.Head, UserList(UserIndex).Char.Heading, UserList(UserIndex).Char.WeaponAnim, UserList(UserIndex).Char.ShieldAnim, UserList(UserIndex).Char.CascoAnim)
                    End If
                Else
                    Call SendData(ToIndex, UserIndex, 0, "||Tu clase no puede usar este objeto." & FONTTYPE_INFO)
                End If
        End Select
End Select

'Actualiza
Call UpdateUserInv(True, UserIndex, 0)


Exit Sub
errhandler:
Call LogError("EquiparInvItem Slot:" & Slot)
End Sub

Private Function CheckRazaUsaRopa(ByVal UserIndex As Integer, ItemIndex As Integer) As Boolean
On Error GoTo errhandler

'Verifica si la raza puede usar la ropa
If UserList(UserIndex).Raza = "Humano" Or _
   UserList(UserIndex).Raza = "Elfo" Or _
   UserList(UserIndex).Raza = "Elfo Oscuro" Then
        CheckRazaUsaRopa = (ObjData(ItemIndex).RazaEnana = 0)
Else
        CheckRazaUsaRopa = (ObjData(ItemIndex).RazaEnana = 1)
End If


Exit Function
errhandler:
    Call LogError("Error CheckRazaUsaRopa ItemIndex:" & ItemIndex)

End Function

Sub UseInvItem(ByVal UserIndex As Integer, ByVal Slot As Byte)

'Usa un item del inventario
Dim Obj As ObjData
Dim ObjIndex As Integer
Dim TargObj As ObjData
Dim MiObj As Obj

If UserList(UserIndex).Invent.Object(Slot).Amount = 0 Then Exit Sub

Obj = ObjData(UserList(UserIndex).Invent.Object(Slot).ObjIndex)

If Obj.Newbie = 1 And Not EsNewbie(UserIndex) Then
    Call SendData(ToIndex, UserIndex, 0, "||Solo los newbies pueden usar estos objetos." & FONTTYPE_INFO)
    Exit Sub
End If

If Obj.ObjType = OBJTYPE_WEAPON Then
    'lo mas probable es q sea un cazador
    If UCase$(UserList(UserIndex).Clase) = "CAZADOR" Then
        If Not IntervaloPermiteUsarArcos(UserIndex) Then Exit Sub
    '2da oportunidad
    ElseIf UCase$(UserList(UserIndex).Clase = "GUERRERO") Then
        If Not IntervaloPermiteUsarArcos(UserIndex) Then Exit Sub
    Else
    'y bue, mala suerte hubo q comparar los 3
        If Not IntervaloPermiteUsar(UserIndex) Then Exit Sub
    End If
Else
    If Not IntervaloPermiteUsar(UserIndex) Then
        Exit Sub
    End If
End If

ObjIndex = UserList(UserIndex).Invent.Object(Slot).ObjIndex
UserList(UserIndex).flags.TargetObjInvIndex = ObjIndex
UserList(UserIndex).flags.TargetObjInvSlot = Slot

Select Case Obj.ObjType

    Case OBJTYPE_USEONCE
        If UserList(UserIndex).flags.Muerto = 1 Then
            Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
            Exit Sub
        End If

        'Usa el item
        Call AddtoVar(UserList(UserIndex).Stats.MinHam, Obj.MinHam, UserList(UserIndex).Stats.MaxHam)
        UserList(UserIndex).flags.Hambre = 0
        Call EnviarHambreYsed(UserIndex)
        'Sonido
        SendData ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SOUND_COMIDA
        
        'Quitamos del inv el item
        Call QuitarUserInvItem(UserIndex, Slot, 1)
        
        Call UpdateUserInv(False, UserIndex, Slot)

            
    Case OBJTYPE_GUITA
    
        If UserList(UserIndex).flags.Muerto = 1 Then
            Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
            Exit Sub
        End If
        
        UserList(UserIndex).Stats.GLD = UserList(UserIndex).Stats.GLD + UserList(UserIndex).Invent.Object(Slot).Amount
        UserList(UserIndex).Invent.Object(Slot).Amount = 0
        UserList(UserIndex).Invent.Object(Slot).ObjIndex = 0
        UserList(UserIndex).Invent.NroItems = UserList(UserIndex).Invent.NroItems - 1
        
        Call UpdateUserInv(False, UserIndex, Slot)
        Call SendUserStatsBox(UserIndex)
        
    Case OBJTYPE_WEAPON
        
        If UserList(UserIndex).flags.Muerto = 1 Then
                Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
                Exit Sub
        End If

    
        If ObjData(ObjIndex).proyectil = 1 Then
            
            Call SendData(ToIndex, UserIndex, 0, "T01" & Proyectiles)

           
        Else
        
            If UserList(UserIndex).flags.TargetObj = 0 Then Exit Sub
            
            TargObj = ObjData(UserList(UserIndex).flags.TargetObj)
            '�El target-objeto es le�a?
            If TargObj.ObjType = OBJTYPE_LE�A Then
                    If UserList(UserIndex).Invent.Object(Slot).ObjIndex = DAGA Then
                        Call TratarDeHacerFogata(UserList(UserIndex).flags.TargetObjMap _
                             , UserList(UserIndex).flags.TargetObjX, UserList(UserIndex).flags.TargetObjY, UserIndex)
                    
                    Else
             
                    End If
            End If
            
        End If
    Case OBJTYPE_POCIONES
    
        If UserList(UserIndex).flags.Muerto = 1 Then
            Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
            Exit Sub
        End If
        
'        If UserList(UserIndex).flags.PuedeAtacar = 0 Then
        If Not IntervaloPermiteAtacar(UserIndex, False) Then
            Call SendData(ToIndex, UserIndex, 0, "||��Debes esperar unos momentos para tomar otra pocion!!" & FONTTYPE_INFO)
            Exit Sub
        End If
        
        UserList(UserIndex).flags.TomoPocion = True
        UserList(UserIndex).flags.TipoPocion = Obj.TipoPocion
                
        Select Case UserList(UserIndex).flags.TipoPocion
        
            Case 1 'Modif la agilidad
                UserList(UserIndex).flags.DuracionEfecto = Obj.DuracionEfecto
        
                'Usa el item
                Call AddtoVar(UserList(UserIndex).Stats.UserAtributos(Agilidad), RandomNumber(Obj.MinModificador, Obj.MaxModificador), MAXATRIBUTOS)
                If UserList(UserIndex).Stats.UserAtributos(Agilidad) > 2 * UserList(UserIndex).Stats.UserAtributosBackUP(Agilidad) Then UserList(UserIndex).Stats.UserAtributos(Agilidad) = 2 * UserList(UserIndex).Stats.UserAtributosBackUP(Agilidad)
                
                'Quitamos del inv el item
                Call QuitarUserInvItem(UserIndex, Slot, 1)
                Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SND_BEBER)
        
            Case 2 'Modif la fuerza
                UserList(UserIndex).flags.DuracionEfecto = Obj.DuracionEfecto
        
                'Usa el item
                Call AddtoVar(UserList(UserIndex).Stats.UserAtributos(Fuerza), RandomNumber(Obj.MinModificador, Obj.MaxModificador), MAXATRIBUTOS)
                If UserList(UserIndex).Stats.UserAtributos(Fuerza) > 2 * UserList(UserIndex).Stats.UserAtributosBackUP(Fuerza) Then UserList(UserIndex).Stats.UserAtributos(Fuerza) = 2 * UserList(UserIndex).Stats.UserAtributosBackUP(Fuerza)
                
                
                'Quitamos del inv el item
                Call QuitarUserInvItem(UserIndex, Slot, 1)
                Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SND_BEBER)
                
            Case 3 'Pocion roja, restaura HP
                'Usa el item
                AddtoVar UserList(UserIndex).Stats.MinHP, RandomNumber(Obj.MinModificador, Obj.MaxModificador), UserList(UserIndex).Stats.MaxHP
                
                'Quitamos del inv el item
                Call QuitarUserInvItem(UserIndex, Slot, 1)
                Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SND_BEBER)
            
            Case 4 'Pocion azul, restaura MANA
                'Usa el item
                Call AddtoVar(UserList(UserIndex).Stats.MinMAN, Porcentaje(UserList(UserIndex).Stats.MaxMAN, 5), UserList(UserIndex).Stats.MaxMAN)
                
                'Quitamos del inv el item
                Call QuitarUserInvItem(UserIndex, Slot, 1)
                Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SND_BEBER)
                
            Case 5 ' Pocion violeta
                If UserList(UserIndex).flags.Envenenado = 1 Then
                    UserList(UserIndex).flags.Envenenado = 0
                    Call SendData(ToIndex, UserIndex, 0, "||Te has curado del envenenamiento." & FONTTYPE_INFO)
                End If
                'Quitamos del inv el item
                Call QuitarUserInvItem(UserIndex, Slot, 1)
                Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SND_BEBER)
            Case 6  ' Pocion Negra
                If UserList(UserIndex).flags.Privilegios = 0 Then
                    Call QuitarUserInvItem(UserIndex, Slot, 1)
                    Call UserDie(UserIndex)
                    Call SendData(ToIndex, UserIndex, 0, "||Sientes un gran mareo y pierdes el conocimiento." & FONTTYPE_FIGHT)
                End If
            Case 7 ' Pocion desparalizante
                If UserList(UserIndex).flags.Paralizado = 1 Then
                    UserList(UserIndex).flags.Paralizado = 0
                    Call SendData(ToIndex, UserIndex, 0, "||Tu paralizacion ha sido quitada." & FONTTYPE_INFO)
                    Call SendData(ToIndex, UserIndex, 0, "PARADOK")
                    'Quitamos del inv el item
                    Call QuitarUserInvItem(UserIndex, Slot, 1)
                    Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SND_BEBER)
                Else
                    Call SendData(ToIndex, UserIndex, 0, "||No estas paralizado." & FONTTYPE_INFO)
                End If
            Case 8 'Poti invisible
                If UserList(UserIndex).flags.Muerto = 1 Then
                    Call SendData(ToIndex, UserIndex, 0, "||No puedes hacerte invisible si estas muerto." & FONTTYPE_INFO)
                    Exit Sub
                Else
                    If UserList(UserIndex).flags.Invisible = 1 Then
                        Call SendData(ToIndex, UserIndex, 0, "||Ya estas invisible." & FONTTYPE_INFO)
                        Exit Sub
                    End If
                    Call SendData(ToIndex, UserIndex, 0, "||Te has hecho invisible." & FONTTYPE_INFO)
                    UserList(UserIndex).flags.Invisible = 1
                    Call SendData(ToMap, 0, UserList(UserIndex).Pos.Map, "NOVER" & UserList(UserIndex).Char.charindex & ",1")
                    Call QuitarUserInvItem(UserIndex, Slot, 1)
                End If
            Case 9 ' Pocion de Resurreccion
                If UserList(UserIndex).flags.Muerto = 1 Then
                    Call RevivirUsuario(UserIndex)
                    Call SendData(ToIndex, UserIndex, 0, "||Te has Resucitado a ti mismo" & FONTTYPE_INFO)
                    Call QuitarUserInvItem(UserIndex, Slot, 1)
                    Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & 20)
                Else
                    Call SendData(ToIndex, UserIndex, 0, "||No puedes usar este objeto estando vivo." & FONTTYPE_INFO)
                End If
       End Select
       Call SendUserStatsBox(UserIndex)
       Call UpdateUserInv(False, UserIndex, Slot)

     Case OBJTYPE_BEBIDA
    
        If UserList(UserIndex).flags.Muerto = 1 Then
            Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
            Exit Sub
        End If
        AddtoVar UserList(UserIndex).Stats.MinAGU, Obj.MinSed, UserList(UserIndex).Stats.MaxAGU
        UserList(UserIndex).flags.Sed = 0
        Call EnviarHambreYsed(UserIndex)
        
        'Quitamos del inv el item
        Call QuitarUserInvItem(UserIndex, Slot, 1)
        
        Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SND_BEBER)
        
        Call UpdateUserInv(False, UserIndex, Slot)
    
    Case OBJTYPE_LLAVES
        
        If UserList(UserIndex).flags.Muerto = 1 Then
            Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
            Exit Sub
        End If
        
        If UserList(UserIndex).flags.TargetObj = 0 Then Exit Sub
        TargObj = ObjData(UserList(UserIndex).flags.TargetObj)
        '�El objeto clickeado es una puerta?
        If TargObj.ObjType = OBJTYPE_PUERTAS Then
            '�Esta cerrada?
            If TargObj.Cerrada = 1 Then
                  '�Cerrada con llave?
                  If TargObj.Llave > 0 Then
                     If TargObj.Clave = Obj.Clave Then
         
                        MapData(UserList(UserIndex).flags.TargetObjMap, UserList(UserIndex).flags.TargetObjX, UserList(UserIndex).flags.TargetObjY).OBJInfo.ObjIndex _
                        = ObjData(MapData(UserList(UserIndex).flags.TargetObjMap, UserList(UserIndex).flags.TargetObjX, UserList(UserIndex).flags.TargetObjY).OBJInfo.ObjIndex).IndexCerrada
                        UserList(UserIndex).flags.TargetObj = MapData(UserList(UserIndex).flags.TargetObjMap, UserList(UserIndex).flags.TargetObjX, UserList(UserIndex).flags.TargetObjY).OBJInfo.ObjIndex
                        Call SendData(ToIndex, UserIndex, 0, "||Has abierto la puerta." & FONTTYPE_INFO)
                        Exit Sub
                     Else
                        Call SendData(ToIndex, UserIndex, 0, "||La llave no sirve." & FONTTYPE_INFO)
                        Exit Sub
                     End If
                  Else
                     If TargObj.Clave = Obj.Clave Then
                        MapData(UserList(UserIndex).flags.TargetObjMap, UserList(UserIndex).flags.TargetObjX, UserList(UserIndex).flags.TargetObjY).OBJInfo.ObjIndex _
                        = ObjData(MapData(UserList(UserIndex).flags.TargetObjMap, UserList(UserIndex).flags.TargetObjX, UserList(UserIndex).flags.TargetObjY).OBJInfo.ObjIndex).IndexCerradaLlave
                        Call SendData(ToIndex, UserIndex, 0, "||Has cerrado con llave la puerta." & FONTTYPE_INFO)
                        UserList(UserIndex).flags.TargetObj = MapData(UserList(UserIndex).flags.TargetObjMap, UserList(UserIndex).flags.TargetObjX, UserList(UserIndex).flags.TargetObjY).OBJInfo.ObjIndex
                        Exit Sub
                     Else
                        Call SendData(ToIndex, UserIndex, 0, "||La llave no sirve." & FONTTYPE_INFO)
                        Exit Sub
                     End If
                  End If
            Else
                  Call SendData(ToIndex, UserIndex, 0, "||No esta cerrada." & FONTTYPE_INFO)
                  Exit Sub
            End If
            
        End If
    
        Case OBJTYPE_BOTELLAVACIA
            If UserList(UserIndex).flags.Muerto = 1 Then
                Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
                Exit Sub
            End If
            If Not HayAgua(UserList(UserIndex).Pos.Map, UserList(UserIndex).flags.TargetX, UserList(UserIndex).flags.TargetY) Then
                Call SendData(ToIndex, UserIndex, 0, "||No hay agua all�." & FONTTYPE_INFO)
                Exit Sub
            End If
            MiObj.Amount = 1
            MiObj.ObjIndex = ObjData(UserList(UserIndex).Invent.Object(Slot).ObjIndex).IndexAbierta
            Call QuitarUserInvItem(UserIndex, Slot, 1)
            If Not MeterItemEnInventario(UserIndex, MiObj) Then
                Call TirarItemAlPiso(UserList(UserIndex).Pos, MiObj)
            End If
            
            Call UpdateUserInv(False, UserIndex, Slot)
    
        Case OBJTYPE_BOTELLALLENA
            If UserList(UserIndex).flags.Muerto = 1 Then
                Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
                Exit Sub
            End If
            AddtoVar UserList(UserIndex).Stats.MinAGU, Obj.MinSed, UserList(UserIndex).Stats.MaxAGU
            UserList(UserIndex).flags.Sed = 0
            Call EnviarHambreYsed(UserIndex)
            MiObj.Amount = 1
            MiObj.ObjIndex = ObjData(UserList(UserIndex).Invent.Object(Slot).ObjIndex).IndexCerrada
            Call QuitarUserInvItem(UserIndex, Slot, 1)
            If Not MeterItemEnInventario(UserIndex, MiObj) Then
                Call TirarItemAlPiso(UserList(UserIndex).Pos, MiObj)
            End If
            
            
        Case OBJTYPE_HERRAMIENTAS
            
            If UserList(UserIndex).flags.Muerto = 1 Then
                Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
                Exit Sub
            End If
            If Not UserList(UserIndex).Stats.MinSta > 0 Then
                Call SendData(ToIndex, UserIndex, 0, "||Estas muy cansado" & FONTTYPE_INFO)
                Exit Sub
            End If
            
            If UserList(UserIndex).Invent.Object(Slot).Equipped = 0 Then
                Call SendData(ToIndex, UserIndex, 0, "||Antes de usar la herramienta deberias equipartela." & FONTTYPE_INFO)
                Exit Sub
            End If
            
            Call AddtoVar(UserList(UserIndex).Reputacion.PlebeRep, vlProleta, MAXREP)
            
            Select Case ObjIndex
                Case OBJTYPE_CA�A, RED_PESCA
                    Call SendData(ToIndex, UserIndex, 0, "T01" & Pesca)
                Case HACHA_LE�ADOR
                    Call SendData(ToIndex, UserIndex, 0, "T01" & Talar)
                Case PIQUETE_MINERO
                    Call SendData(ToIndex, UserIndex, 0, "T01" & Mineria)
                Case MARTILLO_HERRERO
                    Call SendData(ToIndex, UserIndex, 0, "T01" & Herreria)
                Case SERRUCHO_CARPINTERO
                    Call EnivarObjConstruibles(UserIndex)
                    Call SendData(ToIndex, UserIndex, 0, "SFC")

            End Select
        
        Case OBJTYPE_PERGAMINOS
            If UserList(UserIndex).flags.Muerto = 1 Then
                Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
                Exit Sub
            End If
            
            If UserList(UserIndex).flags.Hambre = 0 And _
               UserList(UserIndex).flags.Sed = 0 Then
                Call AgregarHechizo(UserIndex, Slot)
                
                Call UpdateUserInv(False, UserIndex, Slot)

            Else
               Call SendData(ToIndex, UserIndex, 0, "||Estas demasiado hambriento y sediento." & FONTTYPE_INFO)
            End If
       
       Case OBJTYPE_MINERALES
           If UserList(UserIndex).flags.Muerto = 1 Then
                Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
                Exit Sub
           End If
           Call SendData(ToIndex, UserIndex, 0, "T01" & FundirMetal)
       
       Case OBJTYPE_INSTRUMENTOS
            If UserList(UserIndex).flags.Muerto = 1 Then
                Call SendData(ToIndex, UserIndex, 0, "||��Estas muerto!! Solo podes usar items cuando estas vivo. " & FONTTYPE_INFO)
                Exit Sub
            End If
            Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & Obj.Snd1)
       
       Case OBJTYPE_BARCOS
    'Verifica si esta aproximado al agua antes de permitirle navegar
        If UserList(UserIndex).Stats.ELV < 25 Then
            If UCase$(UserList(UserIndex).Clase) <> "PESCADOR" And UCase$(UserList(UserIndex).Clase) <> "PIRATA" Then
                Call SendData(ToIndex, UserIndex, 0, "||Para recorrer los mares debes ser nivel 25 o superior." & FONTTYPE_INFO)
                Exit Sub
            End If
        End If
        If ((LegalPos(UserList(UserIndex).Pos.Map, UserList(UserIndex).Pos.X - 1, UserList(UserIndex).Pos.Y, True) Or _
            LegalPos(UserList(UserIndex).Pos.Map, UserList(UserIndex).Pos.X, UserList(UserIndex).Pos.Y - 1, True) Or _
            LegalPos(UserList(UserIndex).Pos.Map, UserList(UserIndex).Pos.X + 1, UserList(UserIndex).Pos.Y, True) Or _
            LegalPos(UserList(UserIndex).Pos.Map, UserList(UserIndex).Pos.X, UserList(UserIndex).Pos.Y + 1, True)) And _
            UserList(UserIndex).flags.Navegando = 0) _
            Or UserList(UserIndex).flags.Navegando = 1 Then
           Call DoNavega(UserIndex, Obj, Slot)
        Else
            Call SendData(ToIndex, UserIndex, 0, "||�Debes aproximarte al agua para usar el barco!" & FONTTYPE_INFO)
        End If
           
End Select

'Actualiza
'Call SendUserStatsBox(UserIndex)
'Call UpdateUserInv(False, UserIndex, Slot)

End Sub

Sub EnivarArmasConstruibles(ByVal UserIndex As Integer)

Dim i As Integer, cad$

For i = 1 To UBound(ArmasHerrero)
    If ObjData(ArmasHerrero(i)).SkHerreria <= UserList(UserIndex).Stats.UserSkills(Herreria) \ ModHerreriA(UserList(UserIndex).Clase) Then
        If ObjData(ArmasHerrero(i)).ObjType = OBJTYPE_WEAPON Then
        '[DnG!]
            cad$ = cad$ & ObjData(ArmasHerrero(i)).Name & " (" & ObjData(ArmasHerrero(i)).LingH & "-" & ObjData(ArmasHerrero(i)).LingP & "-" & ObjData(ArmasHerrero(i)).LingO & ")" & "," & ArmasHerrero(i) & ","
        '[/DnG!]
        Else
            cad$ = cad$ & ObjData(ArmasHerrero(i)).Name & "," & ArmasHerrero(i) & ","
        End If
    End If
Next i

Call SendData(ToIndex, UserIndex, 0, "LAH" & cad$)

End Sub
 
Sub EnivarObjConstruibles(ByVal UserIndex As Integer)

Dim i As Integer, cad$

For i = 1 To UBound(ObjCarpintero)
    If ObjData(ObjCarpintero(i)).SkCarpinteria <= UserList(UserIndex).Stats.UserSkills(Carpinteria) / ModCarpinteria(UserList(UserIndex).Clase) Then _
        cad$ = cad$ & ObjData(ObjCarpintero(i)).Name & " (" & ObjData(ObjCarpintero(i)).Madera & ")" & "," & ObjCarpintero(i) & ","
Next i

Call SendData(ToIndex, UserIndex, 0, "OBR" & cad$)

End Sub

Sub EnivarArmadurasConstruibles(ByVal UserIndex As Integer)

Dim i As Integer, cad$

For i = 1 To UBound(ArmadurasHerrero)
    If ObjData(ArmadurasHerrero(i)).SkHerreria <= UserList(UserIndex).Stats.UserSkills(Herreria) / ModHerreriA(UserList(UserIndex).Clase) Then
        '[DnG!]
        cad$ = cad$ & ObjData(ArmadurasHerrero(i)).Name & " (" & ObjData(ArmadurasHerrero(i)).LingH & "-" & ObjData(ArmadurasHerrero(i)).LingP & "-" & ObjData(ArmadurasHerrero(i)).LingO & ")" & "," & ArmadurasHerrero(i) & ","
        '[/DnG!]
    End If
Next i

Call SendData(ToIndex, UserIndex, 0, "LAR" & cad$)

End Sub


                   

Sub TirarTodo(ByVal UserIndex As Integer)
On Error Resume Next

If MapData(UserList(UserIndex).Pos.Map, UserList(UserIndex).Pos.X, UserList(UserIndex).Pos.Y).trigger = 6 Then Exit Sub

Call TirarTodosLosItems(UserIndex)
Call TirarOro(UserList(UserIndex).Stats.GLD, UserIndex)

End Sub

Public Function ItemSeCae(ByVal Index As Integer) As Boolean

ItemSeCae = (ObjData(Index).Real <> 1 Or ObjData(Index).NoSeCae = 0) And _
            (ObjData(Index).Caos <> 1 Or ObjData(Index).NoSeCae = 0) And _
            ObjData(Index).ObjType <> OBJTYPE_LLAVES And _
            ObjData(Index).ObjType <> OBJTYPE_BARCOS And _
            ObjData(Index).NoSeCae = 0


End Function

Sub TirarTodosLosItems(ByVal UserIndex As Integer)

'Call LogTarea("Sub TirarTodosLosItems")

Dim i As Byte
Dim NuevaPos As WorldPos
Dim MiObj As Obj
Dim ItemIndex As Integer

For i = 1 To MAX_INVENTORY_SLOTS

  ItemIndex = UserList(UserIndex).Invent.Object(i).ObjIndex
  If ItemIndex > 0 Then
         If ItemSeCae(ItemIndex) Then
                NuevaPos.X = 0
                NuevaPos.Y = 0
                Tilelibre UserList(UserIndex).Pos, NuevaPos
                If NuevaPos.X <> 0 And NuevaPos.Y <> 0 Then
                    If MapData(NuevaPos.Map, NuevaPos.X, NuevaPos.Y).OBJInfo.ObjIndex = 0 Then Call DropObj(UserIndex, i, MAX_INVENTORY_OBJS, NuevaPos.Map, NuevaPos.X, NuevaPos.Y)
                End If
         End If
         
  End If
  
Next i

End Sub


Function ItemNewbie(ByVal ItemIndex As Integer) As Boolean

ItemNewbie = ObjData(ItemIndex).Newbie = 1

End Function

Sub TirarTodosLosItemsNoNewbies(ByVal UserIndex As Integer)
Dim i As Byte
Dim NuevaPos As WorldPos
Dim MiObj As Obj
Dim ItemIndex As Integer

If MapData(UserList(UserIndex).Pos.Map, UserList(UserIndex).Pos.X, UserList(UserIndex).Pos.Y).trigger = 6 Then Exit Sub

For i = 1 To MAX_INVENTORY_SLOTS
  ItemIndex = UserList(UserIndex).Invent.Object(i).ObjIndex
  If ItemIndex > 0 Then
         If ItemSeCae(ItemIndex) And Not ItemNewbie(ItemIndex) Then
                NuevaPos.X = 0
                NuevaPos.Y = 0
                Tilelibre UserList(UserIndex).Pos, NuevaPos
                If NuevaPos.X <> 0 And NuevaPos.Y <> 0 Then
                    If MapData(NuevaPos.Map, NuevaPos.X, NuevaPos.Y).OBJInfo.ObjIndex = 0 Then Call DropObj(UserIndex, i, MAX_INVENTORY_OBJS, NuevaPos.Map, NuevaPos.X, NuevaPos.Y)
                End If
         End If
         
  End If
Next i

End Sub



