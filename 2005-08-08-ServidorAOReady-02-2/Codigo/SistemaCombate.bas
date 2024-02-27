Attribute VB_Name = "SistemaCombate"
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

Public Const MAXDISTANCIAARCO = 18
Public Const MAXDISTANCIAMAGIA = 18

Function ModificadorEvasion(ByVal Clase As String) As Single

Select Case UCase$(Clase)
    Case "GUERRERO"
        ModificadorEvasion = 1
    Case "CAZADOR"
        ModificadorEvasion = 0.9
    Case "PALADIN"
        ModificadorEvasion = 0.9
    Case "BANDIDO"
        ModificadorEvasion = 0.9
    Case "ASESINO"
        ModificadorEvasion = 1.1
    Case "PIRATA"
        ModificadorEvasion = 0.9
    Case "LADRON"
        ModificadorEvasion = 1.1
    Case "BARDO"
        ModificadorEvasion = 1.1
    Case Else
        ModificadorEvasion = 0.8
End Select
End Function

Function ModificadorPoderAtaqueArmas(ByVal Clase As String) As Single
Select Case UCase$(Clase)
    Case "GUERRERO"
        ModificadorPoderAtaqueArmas = 1
    Case "CAZADOR"
        ModificadorPoderAtaqueArmas = 0.8
    Case "PALADIN"
        ModificadorPoderAtaqueArmas = 0.85
    Case "ASESINO"
        ModificadorPoderAtaqueArmas = 0.85
    Case "PIRATA"
        ModificadorPoderAtaqueArmas = 0.8
    Case "LADRON"
        ModificadorPoderAtaqueArmas = 0.75
    Case "BANDIDO"
        ModificadorPoderAtaqueArmas = 0.75
    Case "CLERIGO"
        ModificadorPoderAtaqueArmas = 0.7
    Case "BARDO"
        ModificadorPoderAtaqueArmas = 0.7
    Case "DRUIDA"
        ModificadorPoderAtaqueArmas = 0.7
    Case "PESCADOR"
        ModificadorPoderAtaqueArmas = 0.6
    Case "LE�ADOR"
        ModificadorPoderAtaqueArmas = 0.6
    Case "MINERO"
        ModificadorPoderAtaqueArmas = 0.6
    Case "HERRERO"
        ModificadorPoderAtaqueArmas = 0.6
    Case "CARPINTERO"
        ModificadorPoderAtaqueArmas = 0.6
    Case Else
        ModificadorPoderAtaqueArmas = 0.5
End Select
End Function

Function ModificadorPoderAtaqueProyectiles(ByVal Clase As String) As Single
Select Case UCase$(Clase)
    Case "GUERRERO"
        ModificadorPoderAtaqueProyectiles = 0.8
    Case "CAZADOR"
        ModificadorPoderAtaqueProyectiles = 1
    Case "PALADIN"
        ModificadorPoderAtaqueProyectiles = 0.75
    Case "ASESINO"
        ModificadorPoderAtaqueProyectiles = 0.75
    Case "PIRATA"
        ModificadorPoderAtaqueProyectiles = 0.75
    Case "LADRON"
        ModificadorPoderAtaqueProyectiles = 0.8
    Case "BANDIDO"
        ModificadorPoderAtaqueProyectiles = 0.8
    Case "CLERIGO"
        ModificadorPoderAtaqueProyectiles = 0.7
    Case "BARDO"
        ModificadorPoderAtaqueProyectiles = 0.7
    Case "DRUIDA"
        ModificadorPoderAtaqueProyectiles = 0.75
    Case "PESCADOR"
        ModificadorPoderAtaqueProyectiles = 0.65
    Case "LE�ADOR"
        ModificadorPoderAtaqueProyectiles = 0.7
    Case "MINERO"
        ModificadorPoderAtaqueProyectiles = 0.65
    Case "HERRERO"
        ModificadorPoderAtaqueProyectiles = 0.65
    Case "CARPINTERO"
        ModificadorPoderAtaqueProyectiles = 0.7
    Case Else
        ModificadorPoderAtaqueProyectiles = 0.5
End Select
End Function

Function ModicadorDa�oClaseArmas(ByVal Clase As String) As Single
Select Case UCase$(Clase)
    Case "GUERRERO"
        ModicadorDa�oClaseArmas = 1.1
    Case "CAZADOR"
        ModicadorDa�oClaseArmas = 0.9
    Case "PALADIN"
        ModicadorDa�oClaseArmas = 0.9
    Case "ASESINO"
        ModicadorDa�oClaseArmas = 0.9
    Case "LADRON"
        ModicadorDa�oClaseArmas = 0.8
    Case "PIRATA"
        ModicadorDa�oClaseArmas = 0.8
    Case "BANDIDO"
        ModicadorDa�oClaseArmas = 0.8
    Case "CLERIGO"
        ModicadorDa�oClaseArmas = 0.8
    Case "BARDO"
        ModicadorDa�oClaseArmas = 0.75
    Case "DRUIDA"
        ModicadorDa�oClaseArmas = 0.75
    Case "PESCADOR"
        ModicadorDa�oClaseArmas = 0.6
    Case "LE�ADOR"
        ModicadorDa�oClaseArmas = 0.7
    Case "MINERO"
        ModicadorDa�oClaseArmas = 0.75
    Case "HERRERO"
        ModicadorDa�oClaseArmas = 0.75
    Case "CARPINTERO"
        ModicadorDa�oClaseArmas = 0.7
    Case Else
        ModicadorDa�oClaseArmas = 0.5
End Select
End Function

Function ModicadorDa�oClaseProyectiles(ByVal Clase As String) As Single
Select Case UCase$(Clase)
    Case "GUERRERO"
        ModicadorDa�oClaseProyectiles = 1
    Case "CAZADOR"
        ModicadorDa�oClaseProyectiles = 1.1
    Case "PALADIN"
        ModicadorDa�oClaseProyectiles = 0.8
    Case "ASESINO"
        ModicadorDa�oClaseProyectiles = 0.8
    Case "LADRON"
        ModicadorDa�oClaseProyectiles = 0.75
    Case "PIRATA"
        ModicadorDa�oClaseProyectiles = 0.75
    Case "BANDIDO"
        ModicadorDa�oClaseProyectiles = 0.75
    Case "CLERIGO"
        ModicadorDa�oClaseProyectiles = 0.7
    Case "BARDO"
        ModicadorDa�oClaseProyectiles = 0.7
    Case "DRUIDA"
        ModicadorDa�oClaseProyectiles = 0.75
    Case "PESCADOR"
        ModicadorDa�oClaseProyectiles = 0.6
    Case "LE�ADOR"
        ModicadorDa�oClaseProyectiles = 0.7
    Case "MINERO"
        ModicadorDa�oClaseProyectiles = 0.6
    Case "HERRERO"
        ModicadorDa�oClaseProyectiles = 0.6
    Case "CARPINTERO"
        ModicadorDa�oClaseProyectiles = 0.7
    Case Else
        ModicadorDa�oClaseProyectiles = 0.5
End Select
End Function

Function ModEvasionDeEscudoClase(ByVal Clase As String) As Single

Select Case UCase$(Clase)
Case "GUERRERO"
        ModEvasionDeEscudoClase = 1
    Case "CAZADOR"
        ModEvasionDeEscudoClase = 0.8
    Case "PALADIN"
        ModEvasionDeEscudoClase = 1
    Case "ASESINO"
        ModEvasionDeEscudoClase = 0.8
    Case "LADRON"
        ModEvasionDeEscudoClase = 0.7
    Case "BANDIDO"
        ModEvasionDeEscudoClase = 0.8
    Case "PIRATA"
        ModEvasionDeEscudoClase = 0.75
    Case "CLERIGO"
        ModEvasionDeEscudoClase = 0.85
    Case "BARDO"
        ModEvasionDeEscudoClase = 0.75
    Case "DRUIDA"
        ModEvasionDeEscudoClase = 0.75
    Case "PESCADOR"
        ModEvasionDeEscudoClase = 0.7
    Case "LE�ADOR"
        ModEvasionDeEscudoClase = 0.7
    Case "MINERO"
        ModEvasionDeEscudoClase = 0.7
    Case "HERRERO"
        ModEvasionDeEscudoClase = 0.7
    Case "CARPINTERO"
        ModEvasionDeEscudoClase = 0.7
    Case Else
        ModEvasionDeEscudoClase = 0.6
End Select

End Function
Function Minimo(ByVal A As Single, ByVal b As Single) As Single
If A > b Then
    Minimo = b
    Else: Minimo = A
End If
End Function

Function MinimoInt(ByVal A As Integer, ByVal b As Integer) As Integer
If A > b Then
    MinimoInt = b
    Else: MinimoInt = A
End If
End Function

Function Maximo(ByVal A As Single, ByVal b As Single) As Single
If A > b Then
    Maximo = A
    Else: Maximo = b
End If
End Function

Function MaximoInt(ByVal A As Integer, ByVal b As Integer) As Integer
If A > b Then
    MaximoInt = A
    Else: MaximoInt = b
End If
End Function


Function PoderEvasionEscudo(ByVal UserIndex As Integer) As Long

PoderEvasionEscudo = (UserList(UserIndex).Stats.UserSkills(Defensa) * _
ModEvasionDeEscudoClase(UserList(UserIndex).Clase)) / 2

End Function

Function PoderEvasion(ByVal UserIndex As Integer) As Long
    Dim lTemp As Long
     With UserList(UserIndex)
       lTemp = (.Stats.UserSkills(Tacticas) + _
          .Stats.UserSkills(Tacticas) / 33 * .Stats.UserAtributos(Agilidad)) * _
          ModificadorEvasion(.Clase)
       
        PoderEvasion = (lTemp + (2.5 * Maximo(.Stats.ELV - 12, 0)))
    End With
End Function



'Function PoderEvasion(ByVal UserIndex As Integer) As Long
'Dim PoderEvasionTemp As Long

'If UserList(UserIndex).Stats.UserSkills(Tacticas) < 31 Then
'    PoderEvasionTemp = (UserList(UserIndex).Stats.UserSkills(Tacticas) * _
'    ModificadorEvasion(UserList(UserIndex).Clase))
'ElseIf UserList(UserIndex).Stats.UserSkills(Tacticas) < 61 Then
'        PoderEvasionTemp = ((UserList(UserIndex).Stats.UserSkills(Tacticas) + _
'        UserList(UserIndex).Stats.UserAtributos(Agilidad)) * _
'        ModificadorEvasion(UserList(UserIndex).Clase))
'ElseIf UserList(UserIndex).Stats.UserSkills(Tacticas) < 91 Then
'        PoderEvasionTemp = ((UserList(UserIndex).Stats.UserSkills(Tacticas) + _
'        (2 * UserList(UserIndex).Stats.UserAtributos(Agilidad))) * _
'        ModificadorEvasion(UserList(UserIndex).Clase))
'Else
'        PoderEvasionTemp = ((UserList(UserIndex).Stats.UserSkills(Tacticas) + _
'        (3 * UserList(UserIndex).Stats.UserAtributos(Agilidad))) * _
'        ModificadorEvasion(UserList(UserIndex).Clase))
'End If
'PoderEvasion = (PoderEvasionTemp + (2.5 * Maximo(UserList(UserIndex).Stats.ELV - 12, 0)))
'
'End Function
'
'
'



Function PoderAtaqueArma(ByVal UserIndex As Integer) As Long
Dim PoderAtaqueTemp As Long

If UserList(UserIndex).Stats.UserSkills(Armas) < 31 Then
    PoderAtaqueTemp = (UserList(UserIndex).Stats.UserSkills(Armas) * _
    ModificadorPoderAtaqueArmas(UserList(UserIndex).Clase))
ElseIf UserList(UserIndex).Stats.UserSkills(Armas) < 61 Then
    PoderAtaqueTemp = ((UserList(UserIndex).Stats.UserSkills(Armas) + _
    UserList(UserIndex).Stats.UserAtributos(Agilidad)) * _
    ModificadorPoderAtaqueArmas(UserList(UserIndex).Clase))
ElseIf UserList(UserIndex).Stats.UserSkills(Armas) < 91 Then
    PoderAtaqueTemp = ((UserList(UserIndex).Stats.UserSkills(Armas) + _
    (2 * UserList(UserIndex).Stats.UserAtributos(Agilidad))) * _
    ModificadorPoderAtaqueArmas(UserList(UserIndex).Clase))
Else
   PoderAtaqueTemp = ((UserList(UserIndex).Stats.UserSkills(Armas) + _
   (3 * UserList(UserIndex).Stats.UserAtributos(Agilidad))) * _
   ModificadorPoderAtaqueArmas(UserList(UserIndex).Clase))
End If

PoderAtaqueArma = (PoderAtaqueTemp + (2.5 * Maximo(UserList(UserIndex).Stats.ELV - 12, 0)))
End Function

Function PoderAtaqueProyectil(ByVal UserIndex As Integer) As Long
Dim PoderAtaqueTemp As Long

If UserList(UserIndex).Stats.UserSkills(Proyectiles) < 31 Then
    PoderAtaqueTemp = (UserList(UserIndex).Stats.UserSkills(Proyectiles) * _
    ModificadorPoderAtaqueProyectiles(UserList(UserIndex).Clase))
ElseIf UserList(UserIndex).Stats.UserSkills(Proyectiles) < 61 Then
        PoderAtaqueTemp = ((UserList(UserIndex).Stats.UserSkills(Proyectiles) + _
        UserList(UserIndex).Stats.UserAtributos(Agilidad)) * _
        ModificadorPoderAtaqueProyectiles(UserList(UserIndex).Clase))
ElseIf UserList(UserIndex).Stats.UserSkills(Proyectiles) < 91 Then
        PoderAtaqueTemp = ((UserList(UserIndex).Stats.UserSkills(Proyectiles) + _
        (2 * UserList(UserIndex).Stats.UserAtributos(Agilidad))) * _
        ModificadorPoderAtaqueProyectiles(UserList(UserIndex).Clase))
Else
       PoderAtaqueTemp = ((UserList(UserIndex).Stats.UserSkills(Proyectiles) + _
      (3 * UserList(UserIndex).Stats.UserAtributos(Agilidad))) * _
      ModificadorPoderAtaqueProyectiles(UserList(UserIndex).Clase))
End If

PoderAtaqueProyectil = (PoderAtaqueTemp + (2.5 * Maximo(UserList(UserIndex).Stats.ELV - 12, 0)))

End Function

Function PoderAtaqueWresterling(ByVal UserIndex As Integer) As Long
Dim PoderAtaqueTemp As Long

If UserList(UserIndex).Stats.UserSkills(Wresterling) < 31 Then
    PoderAtaqueTemp = (UserList(UserIndex).Stats.UserSkills(Wresterling) * _
    ModificadorPoderAtaqueArmas(UserList(UserIndex).Clase))
ElseIf UserList(UserIndex).Stats.UserSkills(Wresterling) < 61 Then
        PoderAtaqueTemp = ((UserList(UserIndex).Stats.UserSkills(Wresterling) + _
        UserList(UserIndex).Stats.UserAtributos(Agilidad)) * _
        ModificadorPoderAtaqueArmas(UserList(UserIndex).Clase))
ElseIf UserList(UserIndex).Stats.UserSkills(Wresterling) < 91 Then
        PoderAtaqueTemp = ((UserList(UserIndex).Stats.UserSkills(Wresterling) + _
        (2 * UserList(UserIndex).Stats.UserAtributos(Agilidad))) * _
        ModificadorPoderAtaqueArmas(UserList(UserIndex).Clase))
Else
       PoderAtaqueTemp = ((UserList(UserIndex).Stats.UserSkills(Wresterling) + _
       (3 * UserList(UserIndex).Stats.UserAtributos(Agilidad))) * _
       ModificadorPoderAtaqueArmas(UserList(UserIndex).Clase))
End If

PoderAtaqueWresterling = (PoderAtaqueTemp + (2.5 * Maximo(UserList(UserIndex).Stats.ELV - 12, 0)))

End Function


Public Function UserImpactoNpc(ByVal UserIndex As Integer, ByVal NpcIndex As Integer) As Boolean
Dim PoderAtaque As Long
Dim Arma As Integer
Dim proyectil As Boolean
Dim ProbExito As Long

Arma = UserList(UserIndex).Invent.WeaponEqpObjIndex
If Arma = 0 Then proyectil = False Else proyectil = ObjData(Arma).proyectil = 1

If Arma > 0 Then 'Usando un arma
    If proyectil Then
        PoderAtaque = PoderAtaqueProyectil(UserIndex)
    Else
        PoderAtaque = PoderAtaqueArma(UserIndex)
    End If
Else 'Peleando con pu�os
    PoderAtaque = PoderAtaqueWresterling(UserIndex)
End If


ProbExito = Maximo(10, Minimo(90, 50 + ((PoderAtaque - Npclist(NpcIndex).PoderEvasion) * 0.4)))

UserImpactoNpc = (RandomNumber(1, 100) <= ProbExito)

If UserImpactoNpc Then
    If Arma <> 0 Then
       If proyectil Then
            Call SubirSkill(UserIndex, Proyectiles)
       Else
            Call SubirSkill(UserIndex, Armas)
       End If
    Else
        Call SubirSkill(UserIndex, Wresterling)
    End If
End If


End Function

Public Function NpcImpacto(ByVal NpcIndex As Integer, ByVal UserIndex As Integer) As Boolean
Dim Rechazo As Boolean
Dim ProbRechazo As Long
Dim ProbExito As Long
Dim UserEvasion As Long
Dim NpcPoderAtaque As Long
Dim PoderEvasioEscudo As Long
Dim SkillTacticas As Long
Dim SkillDefensa As Long

UserEvasion = PoderEvasion(UserIndex)
NpcPoderAtaque = Npclist(NpcIndex).PoderAtaque
PoderEvasioEscudo = PoderEvasionEscudo(UserIndex)

SkillTacticas = UserList(UserIndex).Stats.UserSkills(Tacticas)
SkillDefensa = UserList(UserIndex).Stats.UserSkills(Defensa)

'Esta usando un escudo ???
If UserList(UserIndex).Invent.EscudoEqpObjIndex > 0 Then UserEvasion = UserEvasion + PoderEvasioEscudo

ProbExito = Maximo(10, Minimo(90, 50 + ((NpcPoderAtaque - UserEvasion) * 0.4)))

NpcImpacto = (RandomNumber(1, 100) <= ProbExito)

' el usuario esta usando un escudo ???
If UserList(UserIndex).Invent.EscudoEqpObjIndex > 0 Then
   If NpcImpacto = False Then
      ProbRechazo = Maximo(10, Minimo(90, 100 * (SkillDefensa / (SkillDefensa + SkillTacticas))))
      Rechazo = (RandomNumber(1, 100) <= ProbRechazo)
      If Rechazo = True Then
      'Se rechazo el ataque con el escudo
         Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SND_ESCUDO)
         Call SendData(ToIndex, UserIndex, 0, "7")
         Call SubirSkill(UserIndex, Defensa)
      End If
   End If
End If

End Function


Public Function CalcularDa�o(ByVal UserIndex As Integer, Optional ByVal NpcIndex As Integer = 0) As Long
Dim Da�oArma As Long, Da�oUsuario As Long, Arma As ObjData, ModifClase As Single
Dim proyectil As ObjData
Dim Da�oMaxArma As Long

''sacar esto si no queremos q la matadracos mate el dragon si o si
Dim matodragon As Boolean
matodragon = False


If UserList(UserIndex).Invent.WeaponEqpObjIndex > 0 Then
    Arma = ObjData(UserList(UserIndex).Invent.WeaponEqpObjIndex)
    
    
    ' Ataca a un npc?
    If NpcIndex > 0 Then
        
        'Usa la mata dragones?
        If Arma.SubTipo = MATADRAGONES Then ' Usa la matadragones?
            ModifClase = ModicadorDa�oClaseArmas(UserList(UserIndex).Clase)
                If Npclist(NpcIndex).NPCtype = DRAGON Then 'Ataca dragon?
                Da�oArma = RandomNumber(Arma.MinHIT, Arma.MaxHIT)
                Da�oMaxArma = Arma.MaxHIT
                matodragon = True ''sacar esto si no queremos q la matadracos mate el dragon si o si
            Else ' Sino es dragon da�o es 1
                Da�oArma = 1
                Da�oMaxArma = 1
            End If
        Else ' da�o comun
           If Arma.proyectil = 1 Then
                ModifClase = ModicadorDa�oClaseProyectiles(UserList(UserIndex).Clase)
                    Da�oArma = RandomNumber(Arma.MinHIT, Arma.MaxHIT)
                Da�oMaxArma = Arma.MaxHIT
                If Arma.Municion = 1 Then
                    proyectil = ObjData(UserList(UserIndex).Invent.MunicionEqpObjIndex)
                    Da�oArma = Da�oArma + RandomNumber(proyectil.MinHIT, proyectil.MaxHIT)
                    Da�oMaxArma = Arma.MaxHIT
                End If
           Else
                ModifClase = ModicadorDa�oClaseArmas(UserList(UserIndex).Clase)
                    Da�oArma = RandomNumber(Arma.MinHIT, Arma.MaxHIT)
                Da�oMaxArma = Arma.MaxHIT
           End If
        End If
    
    Else ' Ataca usuario
        If Arma.SubTipo = MATADRAGONES Then
            ModifClase = ModicadorDa�oClaseArmas(UserList(UserIndex).Clase)
                Da�oArma = 1 ' Si usa la espada matadragones da�o es 1
            Da�oMaxArma = 1
        Else
           If Arma.proyectil = 1 Then
                ModifClase = ModicadorDa�oClaseProyectiles(UserList(UserIndex).Clase)
                    Da�oArma = RandomNumber(Arma.MinHIT, Arma.MaxHIT)
                    Da�oMaxArma = Arma.MaxHIT
                If Arma.Municion = 1 Then
                    proyectil = ObjData(UserList(UserIndex).Invent.MunicionEqpObjIndex)
                    Da�oArma = Da�oArma + RandomNumber(proyectil.MinHIT, proyectil.MaxHIT)
                    Da�oMaxArma = Arma.MaxHIT
                End If
           Else
                ModifClase = ModicadorDa�oClaseArmas(UserList(UserIndex).Clase)
                    Da�oArma = RandomNumber(Arma.MinHIT, Arma.MaxHIT)
                Da�oMaxArma = Arma.MaxHIT
           End If
        End If
    End If
Else
    CalcularDa�o = CInt(UserList(UserIndex).Stats.MaxHIT / 5)
    Exit Function
End If

Da�oUsuario = RandomNumber(UserList(UserIndex).Stats.MinHIT, UserList(UserIndex).Stats.MaxHIT)

''sacar esto si no queremos q la matadracos mate el dragon si o si
If matodragon Then
    CalcularDa�o = Npclist(NpcIndex).Stats.MinHP + Npclist(NpcIndex).Stats.Def
Else
    CalcularDa�o = (((3 * Da�oArma) + ((Da�oMaxArma / 5) * Maximo(0, (UserList(UserIndex).Stats.UserAtributos(Fuerza) - 15))) + Da�oUsuario) * ModifClase)
End If

End Function

Public Sub UserDa�oNpc(ByVal UserIndex As Integer, ByVal NpcIndex As Integer)
Dim da�o As Long



da�o = CalcularDa�o(UserIndex, NpcIndex)

'esta navegando? si es asi le sumamos el da�o del barco
If UserList(UserIndex).flags.Navegando = 1 Then _
        da�o = da�o + RandomNumber(ObjData(UserList(UserIndex).Invent.BarcoObjIndex).MinHIT, ObjData(UserList(UserIndex).Invent.BarcoObjIndex).MaxHIT)

da�o = da�o - Npclist(NpcIndex).Stats.Def

If da�o < 0 Then da�o = 0

'[KEVIN]
Call SendData(ToIndex, UserIndex, 0, "U2" & da�o)
Call CalcularDarExp(UserIndex, NpcIndex, da�o)
Npclist(NpcIndex).Stats.MinHP = Npclist(NpcIndex).Stats.MinHP - da�o
'[/KEVIN]

If Npclist(NpcIndex).Stats.MinHP > 0 Then
    'Trata de apu�alar por la espalda al enemigo
    If PuedeApu�alar(UserIndex) Then
       Call DoApu�alar(UserIndex, NpcIndex, 0, da�o)
       Call SubirSkill(UserIndex, Apu�alar)
    End If
End If

 
If Npclist(NpcIndex).Stats.MinHP <= 0 Then
          
          ' Si era un Dragon perdemos la espada matadragones
          If Npclist(NpcIndex).NPCtype = DRAGON Then
            Call QuitarObjetos(EspadaMataDragonesIndex, 1, UserIndex)
            If Npclist(NpcIndex).Stats.MaxHP > 100000 Then Call LogDesarrollo(UserList(UserIndex).Name & " mat� un drag�n")
          End If
          
          
          ' Para que las mascotas no sigan intentando luchar y
          ' comiencen a seguir al amo
         
          Dim j As Integer
          For j = 1 To MAXMASCOTAS
                If UserList(UserIndex).MascotasIndex(j) > 0 Then
                    If Npclist(UserList(UserIndex).MascotasIndex(j)).TargetNPC = NpcIndex Then Npclist(UserList(UserIndex).MascotasIndex(j)).TargetNPC = 0
                    Npclist(UserList(UserIndex).MascotasIndex(j)).Movement = SIGUE_AMO
                End If
          Next j
  
          Call MuereNpc(NpcIndex, UserIndex)
End If

End Sub


Public Sub NpcDa�o(ByVal NpcIndex As Integer, ByVal UserIndex As Integer)

Dim da�o As Integer, Lugar As Integer, absorbido As Integer, npcfile As String
Dim antda�o As Integer, defbarco As Integer
Dim Obj As ObjData



da�o = RandomNumber(Npclist(NpcIndex).Stats.MinHIT, Npclist(NpcIndex).Stats.MaxHIT)
antda�o = da�o

If UserList(UserIndex).flags.Navegando = 1 Then
    Obj = ObjData(UserList(UserIndex).Invent.BarcoObjIndex)
    defbarco = RandomNumber(Obj.MinDef, Obj.MaxDef)
End If


Lugar = RandomNumber(1, 6)


Select Case Lugar
  Case bCabeza
        'Si tiene casco absorbe el golpe
        If UserList(UserIndex).Invent.CascoEqpObjIndex > 0 Then
           Obj = ObjData(UserList(UserIndex).Invent.CascoEqpObjIndex)
           absorbido = RandomNumber(Obj.MinDef, Obj.MaxDef)
           absorbido = absorbido + defbarco
           da�o = da�o - absorbido
           If da�o < 1 Then da�o = 1
        End If
  Case Else
        'Si tiene armadura absorbe el golpe
        If UserList(UserIndex).Invent.ArmourEqpObjIndex > 0 Then
           Obj = ObjData(UserList(UserIndex).Invent.ArmourEqpObjIndex)
           absorbido = RandomNumber(Obj.MinDef, Obj.MaxDef)
           absorbido = absorbido + defbarco
           da�o = da�o - absorbido
           If da�o < 1 Then da�o = 1
        End If
End Select

Call SendData(ToIndex, UserIndex, 0, "N2" & Lugar & "," & da�o)

If UserList(UserIndex).flags.Privilegios = 0 Then UserList(UserIndex).Stats.MinHP = UserList(UserIndex).Stats.MinHP - da�o

'Muere el usuario
If UserList(UserIndex).Stats.MinHP <= 0 Then

    Call SendData(ToIndex, UserIndex, 0, "6") ' Le informamos que ha muerto ;)
    
    'Si lo mato un guardia
    If Criminal(UserIndex) And Npclist(NpcIndex).NPCtype = NPCTYPE_GUARDIAS Then
        Call RestarCriminalidad(UserIndex)
        If Not Criminal(UserIndex) And UserList(UserIndex).Faccion.FuerzasCaos = 1 Then Call ExpulsarFaccionCaos(UserIndex)
    End If
    
    If Npclist(NpcIndex).MaestroUser > 0 Then
        Call AllFollowAmo(Npclist(NpcIndex).MaestroUser)
    Else
        'Al matarlo no lo sigue mas
        If Npclist(NpcIndex).Stats.Alineacion = 0 Then
                    Npclist(NpcIndex).Movement = Npclist(NpcIndex).flags.OldMovement
                    Npclist(NpcIndex).Hostile = Npclist(NpcIndex).flags.OldHostil
                    Npclist(NpcIndex).flags.AttackedBy = ""
        End If
    End If
    
    Call UserDie(UserIndex)

End If

End Sub

Public Sub RestarCriminalidad(ByVal UserIndex As Integer)
    'If UserList(UserIndex).Reputacion.AsesinoRep > 0 Then
    '     UserList(UserIndex).Reputacion.AsesinoRep = UserList(UserIndex).Reputacion.AsesinoRep - vlASESINO
    '     If UserList(UserIndex).Reputacion.AsesinoRep < 0 Then UserList(UserIndex).Reputacion.AsesinoRep = 0
    'Else
    If UserList(UserIndex).Reputacion.BandidoRep > 0 Then
         UserList(UserIndex).Reputacion.BandidoRep = UserList(UserIndex).Reputacion.BandidoRep - vlASALTO
         If UserList(UserIndex).Reputacion.BandidoRep < 0 Then UserList(UserIndex).Reputacion.BandidoRep = 0
    ElseIf UserList(UserIndex).Reputacion.LadronesRep > 0 Then
         UserList(UserIndex).Reputacion.LadronesRep = UserList(UserIndex).Reputacion.LadronesRep - (vlCAZADOR * 10)
         If UserList(UserIndex).Reputacion.LadronesRep < 0 Then UserList(UserIndex).Reputacion.LadronesRep = 0
    End If
End Sub


Public Sub CheckPets(ByVal NpcIndex As Integer, ByVal UserIndex As Integer, Optional ByVal CheckElementales As Boolean = True)

Dim j As Integer
For j = 1 To MAXMASCOTAS
    If UserList(UserIndex).MascotasIndex(j) > 0 Then
       If UserList(UserIndex).MascotasIndex(j) <> NpcIndex Then
        If CheckElementales Or (Npclist(UserList(UserIndex).MascotasIndex(j)).Numero <> ELEMENTALFUEGO And Npclist(UserList(UserIndex).MascotasIndex(j)).Numero <> ELEMENTALTIERRA) Then
            If Npclist(UserList(UserIndex).MascotasIndex(j)).TargetNPC = 0 Then Npclist(UserList(UserIndex).MascotasIndex(j)).TargetNPC = NpcIndex
            'Npclist(UserList(UserIndex).MascotasIndex(j)).Flags.OldMovement = Npclist(UserList(UserIndex).MascotasIndex(j)).Movement
            Npclist(UserList(UserIndex).MascotasIndex(j)).Movement = NPC_ATACA_NPC
        End If
       End If
    End If
Next j

End Sub
Public Sub AllFollowAmo(ByVal UserIndex As Integer)
Dim j As Integer
For j = 1 To MAXMASCOTAS
    If UserList(UserIndex).MascotasIndex(j) > 0 Then
        Call FollowAmo(UserList(UserIndex).MascotasIndex(j))
    End If
Next j
End Sub

Public Sub NpcAtacaUser(ByVal NpcIndex As Integer, ByVal UserIndex As Integer)
' El npc puede atacar ???
If Npclist(NpcIndex).CanAttack = 1 Then
    Call CheckPets(NpcIndex, UserIndex, False)

    If Npclist(NpcIndex).Target = 0 Then Npclist(NpcIndex).Target = UserIndex

    If UserList(UserIndex).flags.AtacadoPorNpc = 0 And _
       UserList(UserIndex).flags.AtacadoPorUser = 0 Then UserList(UserIndex).flags.AtacadoPorNpc = NpcIndex
Else
    Exit Sub
End If

Npclist(NpcIndex).CanAttack = 0

If Npclist(NpcIndex).flags.Snd1 > 0 Then Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & Npclist(NpcIndex).flags.Snd1)

If NpcImpacto(NpcIndex, UserIndex) Then
    Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SND_IMPACTO)
    
    If UserList(UserIndex).flags.Meditando = False Then
        If UserList(UserIndex).flags.Navegando = 0 Then Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "CFX" & UserList(UserIndex).Char.charindex & "," & FXSANGRE & "," & 0)
    End If
    Call NpcDa�o(NpcIndex, UserIndex)
    '�Puede envenenar?
    If Npclist(NpcIndex).Veneno = 1 Then Call NpcEnvenenarUser(UserIndex)
Else
    Call SendData(ToIndex, UserIndex, 0, "N1")
End If

'-----Tal vez suba los skills------
Call SubirSkill(UserIndex, Tacticas)

Call SendUserStatsBox(UserIndex)
'Controla el nivel del usuario
Call CheckUserLevel(UserIndex)

End Sub

Function NpcImpactoNpc(ByVal Atacante As Integer, ByVal Victima As Integer) As Boolean
Dim PoderAtt As Long, PoderEva As Long, dif As Long
Dim ProbExito As Long

PoderAtt = Npclist(Atacante).PoderAtaque
PoderEva = Npclist(Victima).PoderEvasion
ProbExito = Maximo(10, Minimo(90, 50 + _
            ((PoderAtt - PoderEva) * 0.4)))
NpcImpactoNpc = (RandomNumber(1, 100) <= ProbExito)


End Function

Public Sub NpcDa�oNpc(ByVal Atacante As Integer, ByVal Victima As Integer)
Dim da�o As Integer
Dim ANpc As npc, DNpc As npc
ANpc = Npclist(Atacante)

da�o = RandomNumber(ANpc.Stats.MinHIT, ANpc.Stats.MaxHIT)
Npclist(Victima).Stats.MinHP = Npclist(Victima).Stats.MinHP - da�o

If Npclist(Victima).Stats.MinHP < 1 Then
        
        If Npclist(Atacante).flags.AttackedBy <> "" Then
            Npclist(Atacante).Movement = Npclist(Atacante).flags.OldMovement
            Npclist(Atacante).Hostile = Npclist(Atacante).flags.OldHostil
        Else
            Npclist(Atacante).Movement = Npclist(Atacante).flags.OldMovement
        End If
        
        Call FollowAmo(Atacante)
        
        Call MuereNpc(Victima, Npclist(Atacante).MaestroUser)
End If

End Sub

Public Sub NpcAtacaNpc(ByVal Atacante As Integer, ByVal Victima As Integer, Optional ByVal cambiarMOvimiento As Boolean = True)

' El npc puede atacar ???
If Npclist(Atacante).CanAttack = 1 Then
       Npclist(Atacante).CanAttack = 0
        If cambiarMOvimiento Then
            Npclist(Victima).TargetNPC = Atacante
            Npclist(Victima).Movement = NPC_ATACA_NPC
        End If
Else
    Exit Sub
End If

If Npclist(Atacante).flags.Snd1 > 0 Then Call SendData(ToNPCArea, Atacante, Npclist(Atacante).Pos.Map, "TW" & Npclist(Atacante).flags.Snd1)

If NpcImpactoNpc(Atacante, Victima) Then
    
    If Npclist(Victima).flags.Snd2 > 0 Then
        Call SendData(ToNPCArea, Victima, Npclist(Victima).Pos.Map, "TW" & Npclist(Victima).flags.Snd2)
    Else
        Call SendData(ToNPCArea, Victima, Npclist(Victima).Pos.Map, "TW" & SND_IMPACTO2)
    End If

    If Npclist(Atacante).MaestroUser > 0 Then
        Call SendData(ToNPCArea, Atacante, Npclist(Atacante).Pos.Map, "TW" & SND_IMPACTO)
    Else
        Call SendData(ToNPCArea, Victima, Npclist(Victima).Pos.Map, "TW" & SND_IMPACTO)
    End If
    Call NpcDa�oNpc(Atacante, Victima)
    
Else
    If Npclist(Atacante).MaestroUser > 0 Then
        Call SendData(ToNPCArea, Atacante, Npclist(Atacante).Pos.Map, "TW" & SOUND_SWING)
    Else
        Call SendData(ToNPCArea, Victima, Npclist(Victima).Pos.Map, "TW" & SOUND_SWING)
    End If
End If

End Sub

Public Sub UsuarioAtacaNpc(ByVal UserIndex As Integer, ByVal NpcIndex As Integer)

If Distancia(UserList(UserIndex).Pos, Npclist(NpcIndex).Pos) > MAXDISTANCIAARCO Then
   Call SendData(ToIndex, UserIndex, 0, "||Est�s muy lejos para disparar." & FONTTYPE_FIGHT)
   Exit Sub
End If

If UserList(UserIndex).flags.Seguro And Npclist(NpcIndex).MaestroUser <> 0 Then
    If Not Criminal(Npclist(NpcIndex).MaestroUser) Then
        Call SendData(ToIndex, UserIndex, 0, "||Debes sacar el seguro antes de poder atacar una mascota de un ciudadano." & FONTTYPE_WARNING)
        Exit Sub
    End If
End If

If UserList(UserIndex).Faccion.ArmadaReal = 1 And Npclist(NpcIndex).MaestroUser <> 0 Then
    If Not Criminal(Npclist(NpcIndex).MaestroUser) Then
        Call SendData(ToIndex, UserIndex, 0, "||Los soldados del Ejercito Real tienen prohibido atacar ciudadanos y sus macotas." & FONTTYPE_WARNING)
        Exit Sub
    End If
End If

If Npclist(NpcIndex).NPCtype = 2 And UserList(UserIndex).flags.Seguro Then
    Call SendData(ToIndex, UserIndex, 0, "||Debes quitar el seguro para atacar guardias." & FONTTYPE_FIGHT)
    Exit Sub
End If


Call NpcAtacado(NpcIndex, UserIndex)

If UserImpactoNpc(UserIndex, NpcIndex) Then
    
    If Npclist(NpcIndex).flags.Snd2 > 0 Then
        Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & Npclist(NpcIndex).flags.Snd2)
    Else
        Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SND_IMPACTO2)
    End If
    
    
    
    
    Call UserDa�oNpc(UserIndex, NpcIndex)
   
Else
    Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SOUND_SWING)
    Call SendData(ToIndex, UserIndex, 0, "U1")
End If

End Sub

Public Sub UsuarioAtaca(ByVal UserIndex As Integer)

'If UserList(UserIndex).flags.PuedeAtacar = 1 Then
If IntervaloPermiteAtacar(UserIndex) Then
    
    'Quitamos stamina
    If UserList(UserIndex).Stats.MinSta >= 10 Then
        Call QuitarSta(UserIndex, RandomNumber(1, 10))
    Else
        Call SendData(ToIndex, UserIndex, 0, "||Estas muy cansado para luchar." & FONTTYPE_INFO)
        Exit Sub
    End If
    
    'UserList(UserIndex).flags.PuedeAtacar = 0
    
    Dim AttackPos As WorldPos
    AttackPos = UserList(UserIndex).Pos
    Call HeadtoPos(UserList(UserIndex).Char.Heading, AttackPos)
    
    'Exit if not legal
    If AttackPos.X < XMinMapSize Or AttackPos.X > XMaxMapSize Or AttackPos.Y <= YMinMapSize Or AttackPos.Y > YMaxMapSize Then
        Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SOUND_SWING)
        Exit Sub
    End If
    
    Dim Index As Integer
    Index = MapData(AttackPos.Map, AttackPos.X, AttackPos.Y).UserIndex
        
    'Look for user
    If Index > 0 Then
        Call UsuarioAtacaUsuario(UserIndex, MapData(AttackPos.Map, AttackPos.X, AttackPos.Y).UserIndex)
        Call SendUserStatsBox(UserIndex)
        Call SendUserStatsBox(MapData(AttackPos.Map, AttackPos.X, AttackPos.Y).UserIndex)
        Exit Sub
    End If
    
    'Look for NPC
    If MapData(AttackPos.Map, AttackPos.X, AttackPos.Y).NpcIndex > 0 Then
    
        If Npclist(MapData(AttackPos.Map, AttackPos.X, AttackPos.Y).NpcIndex).Attackable Then
            
            If Npclist(MapData(AttackPos.Map, AttackPos.X, AttackPos.Y).NpcIndex).MaestroUser > 0 And _
               MapInfo(Npclist(MapData(AttackPos.Map, AttackPos.X, AttackPos.Y).NpcIndex).Pos.Map).Pk = False Then
                    Call SendData(ToIndex, UserIndex, 0, "||No pod�s atacar mascotas en zonas seguras" & FONTTYPE_FIGHT)
                    Exit Sub
            End If

            Call UsuarioAtacaNpc(UserIndex, MapData(AttackPos.Map, AttackPos.X, AttackPos.Y).NpcIndex)
            
        Else
            Call SendData(ToIndex, UserIndex, 0, "||No pod�s atacar a este NPC" & FONTTYPE_FIGHT)
        End If
        
        Call SendUserStatsBox(UserIndex)
        
        Exit Sub
    End If
    
    
    Call SendData(ToPCArea, UserIndex, UserList(UserIndex).Pos.Map, "TW" & SOUND_SWING)
    Call SendUserStatsBox(UserIndex)
End If

'[Barrin 30-11-03]
UserList(UserIndex).flags.Trabajando = False
'[/Barrin 30-11-03]

End Sub

Public Function UsuarioImpacto(ByVal AtacanteIndex As Integer, ByVal VictimaIndex As Integer) As Boolean

Dim ProbRechazo As Long
Dim Rechazo As Boolean
Dim ProbExito As Long
Dim PoderAtaque As Long
Dim UserPoderEvasion As Long
Dim UserPoderEvasionEscudo As Long
Dim Arma As Integer
Dim proyectil As Boolean
Dim SkillTacticas As Long
Dim SkillDefensa As Long

SkillTacticas = UserList(VictimaIndex).Stats.UserSkills(Tacticas)
SkillDefensa = UserList(VictimaIndex).Stats.UserSkills(Defensa)

Arma = UserList(AtacanteIndex).Invent.WeaponEqpObjIndex
If Arma > 0 Then
    proyectil = ObjData(Arma).proyectil = 1
Else
    proyectil = False
End If

'Calculamos el poder de evasion...
UserPoderEvasion = PoderEvasion(VictimaIndex)

If UserList(VictimaIndex).Invent.EscudoEqpObjIndex > 0 Then
   UserPoderEvasionEscudo = PoderEvasionEscudo(VictimaIndex)
   UserPoderEvasion = UserPoderEvasion + UserPoderEvasionEscudo
Else
    UserPoderEvasionEscudo = 0
End If

'Esta usando un arma ???
If UserList(AtacanteIndex).Invent.WeaponEqpObjIndex > 0 Then
    
    If proyectil Then
        PoderAtaque = PoderAtaqueProyectil(AtacanteIndex)
    Else
        PoderAtaque = PoderAtaqueArma(AtacanteIndex)
    End If
    ProbExito = Maximo(10, Minimo(90, 50 + _
                ((PoderAtaque - UserPoderEvasion) * 0.4)))
   
Else
    PoderAtaque = PoderAtaqueWresterling(AtacanteIndex)
    ProbExito = Maximo(10, Minimo(90, 50 + _
                ((PoderAtaque - UserPoderEvasion) * 0.4)))
    
End If
UsuarioImpacto = (RandomNumber(1, 100) <= ProbExito)

' el usuario esta usando un escudo ???
If UserList(VictimaIndex).Invent.EscudoEqpObjIndex > 0 Then
    
    'Fallo ???
    If UsuarioImpacto = False Then
      ProbRechazo = Maximo(10, Minimo(90, 100 * (SkillDefensa / (SkillDefensa + SkillTacticas))))
      Rechazo = (RandomNumber(1, 100) <= ProbRechazo)
      If Rechazo = True Then
      'Se rechazo el ataque con el escudo
              Call SendData(ToPCArea, AtacanteIndex, UserList(AtacanteIndex).Pos.Map, "TW" & SND_ESCUDO)
              Call SendData(ToIndex, AtacanteIndex, 0, "8")
              Call SendData(ToIndex, VictimaIndex, 0, "7")
              Call SubirSkill(VictimaIndex, Defensa)
      End If
    End If
End If
    
If UsuarioImpacto Then
   If Arma > 0 Then
           If Not proyectil Then
                  Call SubirSkill(AtacanteIndex, Armas)
           Else
                  Call SubirSkill(AtacanteIndex, Proyectiles)
           End If
   Else
        Call SubirSkill(AtacanteIndex, Wresterling)
   End If
End If

End Function

Public Sub UsuarioAtacaUsuario(ByVal AtacanteIndex As Integer, ByVal VictimaIndex As Integer)

If Not PuedeAtacar(AtacanteIndex, VictimaIndex) Then Exit Sub

If Distancia(UserList(AtacanteIndex).Pos, UserList(VictimaIndex).Pos) > MAXDISTANCIAARCO Then
   Call SendData(ToIndex, AtacanteIndex, 0, "||Est�s muy lejos para disparar." & FONTTYPE_FIGHT)
   Exit Sub
End If


Call UsuarioAtacadoPorUsuario(AtacanteIndex, VictimaIndex)

If UsuarioImpacto(AtacanteIndex, VictimaIndex) Then
    Call SendData(ToPCArea, AtacanteIndex, UserList(AtacanteIndex).Pos.Map, "TW" & SND_IMPACTO)
    
    If UserList(VictimaIndex).flags.Navegando = 0 Then Call SendData(ToPCArea, VictimaIndex, UserList(VictimaIndex).Pos.Map, "CFX" & UserList(VictimaIndex).Char.charindex & "," & FXSANGRE & "," & 0)
    
    Call UserDa�oUser(AtacanteIndex, VictimaIndex)
Else
    Call SendData(ToPCArea, AtacanteIndex, UserList(AtacanteIndex).Pos.Map, "TW" & SOUND_SWING)
    Call SendData(ToIndex, AtacanteIndex, 0, "U1")
    Call SendData(ToIndex, VictimaIndex, 0, "U3" & UserList(AtacanteIndex).Name)
End If

If UCase$(UserList(AtacanteIndex).Clase) = "LADRON" Then Call Desarmar(AtacanteIndex, VictimaIndex)

End Sub

Public Sub UserDa�oUser(ByVal AtacanteIndex As Integer, ByVal VictimaIndex As Integer)
Dim da�o As Long, antda�o As Integer
Dim Lugar As Integer, absorbido As Long
Dim defbarco As Integer

Dim Obj As ObjData

da�o = CalcularDa�o(AtacanteIndex)
antda�o = da�o

Call UserEnvenena(AtacanteIndex, VictimaIndex)

If UserList(AtacanteIndex).flags.Navegando = 1 Then
     Obj = ObjData(UserList(AtacanteIndex).Invent.BarcoObjIndex)
     da�o = da�o + RandomNumber(Obj.MinHIT, Obj.MaxHIT)
End If

If UserList(VictimaIndex).flags.Navegando = 1 Then
     Obj = ObjData(UserList(VictimaIndex).Invent.BarcoObjIndex)
     defbarco = RandomNumber(Obj.MinDef, Obj.MaxDef)
End If

Dim Resist As Byte
If UserList(AtacanteIndex).Invent.WeaponEqpObjIndex > 0 Then
    Resist = ObjData(UserList(AtacanteIndex).Invent.WeaponEqpObjIndex).Refuerzo
End If

Lugar = RandomNumber(1, 6)

Select Case Lugar
  
  Case bCabeza
        'Si tiene casco absorbe el golpe
        If UserList(VictimaIndex).Invent.CascoEqpObjIndex > 0 Then
           Obj = ObjData(UserList(VictimaIndex).Invent.CascoEqpObjIndex)
           absorbido = RandomNumber(Obj.MinDef, Obj.MaxDef)
           absorbido = absorbido + defbarco - Resist
           da�o = da�o - absorbido
           If da�o < 0 Then da�o = 1
        End If
  Case Else
        'Si tiene armadura absorbe el golpe
        If UserList(VictimaIndex).Invent.ArmourEqpObjIndex > 0 Then
           Obj = ObjData(UserList(VictimaIndex).Invent.ArmourEqpObjIndex)
           absorbido = RandomNumber(Obj.MinDef, Obj.MaxDef)
           absorbido = absorbido + defbarco - Resist
           da�o = da�o - absorbido
           If da�o < 0 Then da�o = 1
        End If
End Select

Call SendData(ToIndex, AtacanteIndex, 0, "N5" & Lugar & "," & da�o & "," & UserList(VictimaIndex).Name)
Call SendData(ToIndex, VictimaIndex, 0, "N4" & Lugar & "," & da�o & "," & UserList(AtacanteIndex).Name)

UserList(VictimaIndex).Stats.MinHP = UserList(VictimaIndex).Stats.MinHP - da�o

If UserList(AtacanteIndex).flags.Hambre = 0 And UserList(AtacanteIndex).flags.Sed = 0 Then
        'Si usa un arma quizas suba "Combate con armas"
        If UserList(AtacanteIndex).Invent.WeaponEqpObjIndex > 0 Then
                Call SubirSkill(AtacanteIndex, Armas)
        Else
        'sino tal vez lucha libre
                Call SubirSkill(AtacanteIndex, Wresterling)
        End If
        
        Call SubirSkill(AtacanteIndex, Tacticas)
        
        'Trata de apu�alar por la espalda al enemigo
        If PuedeApu�alar(AtacanteIndex) Then
                Call DoApu�alar(AtacanteIndex, 0, VictimaIndex, da�o)
                Call SubirSkill(AtacanteIndex, Apu�alar)
        End If
End If


If UserList(VictimaIndex).Stats.MinHP <= 0 Then
     
     Call ContarMuerte(VictimaIndex, AtacanteIndex)
     
     ' Para que las mascotas no sigan intentando luchar y
     ' comiencen a seguir al amo
     Dim j As Integer
     For j = 1 To MAXMASCOTAS
        If UserList(AtacanteIndex).MascotasIndex(j) > 0 Then
            If Npclist(UserList(AtacanteIndex).MascotasIndex(j)).Target = VictimaIndex Then Npclist(UserList(AtacanteIndex).MascotasIndex(j)).Target = 0
            Call FollowAmo(UserList(AtacanteIndex).MascotasIndex(j))
        End If
     Next j

     Call ActStats(VictimaIndex, AtacanteIndex)
End If
        

'Controla el nivel del usuario
Call CheckUserLevel(AtacanteIndex)


End Sub

Sub UsuarioAtacadoPorUsuario(ByVal AttackerIndex As Integer, ByVal VictimIndex As Integer)

If TriggerZonaPelea(AttackerIndex, VictimIndex) = TRIGGER6_PERMITE Then Exit Sub

If UserList(AttackerIndex).GuildInfo.GuildName = "" Or UserList(VictimIndex).GuildInfo.GuildName = "" Then

    If Not Criminal(AttackerIndex) And Not Criminal(VictimIndex) Then
        Call VolverCriminal(AttackerIndex)
    End If
    
    If Not Criminal(VictimIndex) Then
          Call AddtoVar(UserList(AttackerIndex).Reputacion.BandidoRep, vlASALTO, MAXREP)
    Else
          Call AddtoVar(UserList(AttackerIndex).Reputacion.NobleRep, vlNoble, MAXREP)
    End If
    
    
Else 'Tiene clan

    If Not UserList(AttackerIndex).GuildRef.IsEnemy(UserList(VictimIndex).GuildInfo.GuildName) Then
        
            If Not Criminal(AttackerIndex) And Not Criminal(VictimIndex) Then
                    Call VolverCriminal(AttackerIndex)
            End If
            
            If Not Criminal(VictimIndex) Then
                  Call AddtoVar(UserList(AttackerIndex).Reputacion.BandidoRep, vlASALTO, MAXREP)
            Else
                  Call AddtoVar(UserList(AttackerIndex).Reputacion.NobleRep, vlNoble, MAXREP)
            End If
            
    Else
            
            If Not Criminal(VictimIndex) Then
                  Call AddtoVar(UserList(AttackerIndex).Reputacion.BandidoRep, vlASALTO, MAXREP)
            Else
                  Call AddtoVar(UserList(AttackerIndex).Reputacion.NobleRep, vlNoble, MAXREP)
            End If
            
            'Call GiveGuildPoints(1, AttackerIndex, False)
    
    End If
    

End If

Call AllMascotasAtacanUser(AttackerIndex, VictimIndex)
Call AllMascotasAtacanUser(VictimIndex, AttackerIndex)


End Sub

Sub AllMascotasAtacanUser(ByVal Victim As Integer, ByVal Maestro As Integer)
'Reaccion de las mascotas
Dim iCount As Integer

For iCount = 1 To MAXMASCOTAS
    If UserList(Maestro).MascotasIndex(iCount) > 0 Then
            Npclist(UserList(Maestro).MascotasIndex(iCount)).flags.AttackedBy = UserList(Victim).Name
            Npclist(UserList(Maestro).MascotasIndex(iCount)).Movement = NPCDEFENSA
            Npclist(UserList(Maestro).MascotasIndex(iCount)).Hostile = 1
    End If
Next iCount

End Sub

Public Function PuedeAtacar(ByVal AttackerIndex As Integer, ByVal VictimIndex As Integer) As Boolean

Dim t As eTrigger6

If UserList(VictimIndex).flags.Muerto = 1 Then
    SendData ToIndex, AttackerIndex, 0, "||No podes atacar a un espiritu" & FONTTYPE_INFO
    PuedeAtacar = False
    Exit Function
End If

t = TriggerZonaPelea(AttackerIndex, VictimIndex)

If t = TRIGGER6_PERMITE Then
    PuedeAtacar = True
    Exit Function
ElseIf t = TRIGGER6_PROHIBE Then
    PuedeAtacar = False
    Exit Function
End If


If MapInfo(UserList(VictimIndex).Pos.Map).Pk = False Then
    Call SendData(ToIndex, AttackerIndex, 0, "||Esta es una zona segura, aqui no podes atacar otros usuarios." & FONTTYPE_WARNING)
    PuedeAtacar = False
    Exit Function
End If

If MapData(UserList(VictimIndex).Pos.Map, UserList(VictimIndex).Pos.X, UserList(VictimIndex).Pos.Y).trigger = TRIGGER_ZONASEGURA Then
    Call SendData(ToIndex, AttackerIndex, 0, "||No podes pelear aqui." & FONTTYPE_WARNING)
    PuedeAtacar = False
    Exit Function
End If

If (Not Criminal(VictimIndex)) And (UserList(AttackerIndex).Faccion.ArmadaReal = 1) Then
    Call SendData(ToIndex, AttackerIndex, 0, "||Los soldados del Ejercito Real tienen prohibido atacar ciudadanos." & FONTTYPE_WARNING)
    PuedeAtacar = False
    Exit Function
End If

'[Barrin 17-12-03]
If (UserList(VictimIndex).Faccion.FuerzasCaos = 1) And (UserList(AttackerIndex).Faccion.FuerzasCaos = 1) Then
    Call SendData(ToIndex, AttackerIndex, 0, "||Los miembros de la legi�n oscura tienen prohibido atacarse entre s�." & FONTTYPE_WARNING)
    PuedeAtacar = False
    Exit Function
End If
'[/Barrin]

If UserList(AttackerIndex).flags.Privilegios = 1 Then
    PuedeAtacar = False
    Exit Function
End If

'Se asegura que la victima no es un GM
If UserList(VictimIndex).flags.Privilegios >= 1 Then
    SendData ToIndex, AttackerIndex, 0, "||��No podes atacar a los administradores del juego!! " & FONTTYPE_WARNING
    PuedeAtacar = False
    Exit Function
End If

If UserList(AttackerIndex).flags.Muerto = 1 Then
    SendData ToIndex, AttackerIndex, 0, "||No podes atacar porque estas muerto" & FONTTYPE_INFO
    PuedeAtacar = False
    Exit Function
End If

If UserList(AttackerIndex).flags.Seguro Then
        If Not Criminal(VictimIndex) Then
                Call SendData(ToIndex, AttackerIndex, 0, "||No podes atacar ciudadanos, para hacerlo debes desactivar el seguro apretando la tecla S" & FONTTYPE_FIGHT)
                Exit Function
        End If
End If
   

PuedeAtacar = True

End Function


Public Function PuedeAtacarNPC(ByVal AttackerIndex As Integer, ByVal NpcIndex As Integer) As Boolean

If Npclist(NpcIndex).MaestroUser > 0 Then
    If Not Criminal(AttackerIndex) And Not Criminal(Npclist(NpcIndex).MaestroUser) Then
        If UserList(AttackerIndex).flags.Seguro Then
            Call SendData(ToIndex, AttackerIndex, 0, "||Para atacar mascotas de ciudadanos debes quitarte el seguro" & FONTTYPE_FIGHT)
            PuedeAtacarNPC = False
            Exit Function
        End If
    End If
End If

If UserList(AttackerIndex).flags.Muerto = 1 Then
    SendData ToIndex, AttackerIndex, 0, "||No podes atacar porque estas muerto" & FONTTYPE_INFO
    PuedeAtacarNPC = False
    Exit Function
End If

If UserList(AttackerIndex).flags.Privilegios = 1 Then
    PuedeAtacarNPC = False
    Exit Function
End If


PuedeAtacarNPC = True

End Function


'[KEVIN]
'
'[Alejo]
'Modifique un poco el sistema de exp por golpe, ahora
'son 2/3 de la exp mientras esta vivo, el resto se
'obtiene al matarlo.
'Ahora adem�s
Sub CalcularDarExp(ByVal UserIndex As Integer, ByVal NpcIndex As Integer, ByVal ElDa�o As Long)

Dim ExpSinMorir As Long
Dim ExpaDar As Long
Dim TotalNpcVida As Long

If ElDa�o <= 0 Then ElDa�o = 0

'2/3 de la experiencia se dan cuando se le golpea, el resto
'se obtiene al matarlo
ExpSinMorir = (2 * Npclist(NpcIndex).GiveEXP) / 3

TotalNpcVida = Npclist(NpcIndex).Stats.MaxHP
If TotalNpcVida <= 0 Then Exit Sub

If ElDa�o > Npclist(NpcIndex).Stats.MinHP Then ElDa�o = Npclist(NpcIndex).Stats.MinHP

'totalnpcvida _____ ExpSinMorir
'da�o         _____ (da�o * ExpSinMorir) / totalNpcVida

ExpaDar = CLng((ElDa�o) * (ExpSinMorir / TotalNpcVida))
If ExpaDar <= 0 Then Exit Sub

If ExpaDar > Npclist(NpcIndex).flags.ExpCount Then
    ExpaDar = Npclist(NpcIndex).flags.ExpCount
    Npclist(NpcIndex).flags.ExpCount = 0
Else
    Npclist(NpcIndex).flags.ExpCount = Npclist(NpcIndex).flags.ExpCount - ExpaDar
End If

If ExpaDar > 0 Then
    If UserList(UserIndex).PartyIndex > 0 Then
        Call mdParty.ObtenerExito(UserIndex, ExpaDar, Npclist(NpcIndex).Pos.Map, Npclist(NpcIndex).Pos.X, Npclist(NpcIndex).Pos.Y)
    Else
        Call AddtoVar(UserList(UserIndex).Stats.Exp, ExpaDar, MAXEXP)
        Call SendData(ToIndex, UserIndex, 0, "||Has ganado " & ExpaDar & " puntos de experiencia." & FONTTYPE_FIGHT)
    End If

    
    Call CheckUserLevel(UserIndex)
End If

'[/KEVIN]
End Sub

Public Function TriggerZonaPelea(ByVal Origen As Integer, ByVal Destino As Integer) As eTrigger6

If Origen > 0 And Destino > 0 And Origen <= UBound(UserList) And Destino <= UBound(UserList) Then
    If MapData(UserList(Origen).Pos.Map, UserList(Origen).Pos.X, UserList(Origen).Pos.Y).trigger = TRIGGER_ZONAPELEA Or _
        MapData(UserList(Destino).Pos.Map, UserList(Destino).Pos.X, UserList(Destino).Pos.Y).trigger = TRIGGER_ZONAPELEA Then
        If (MapData(UserList(Origen).Pos.Map, UserList(Origen).Pos.X, UserList(Origen).Pos.Y).trigger = MapData(UserList(Destino).Pos.Map, UserList(Destino).Pos.X, UserList(Destino).Pos.Y).trigger) Then
            TriggerZonaPelea = TRIGGER6_PERMITE
        Else
            TriggerZonaPelea = TRIGGER6_PROHIBE
        End If
    Else
        TriggerZonaPelea = TRIGGER6_AUSENTE
    End If
Else
    TriggerZonaPelea = TRIGGER6_AUSENTE
End If

End Function

Sub UserEnvenena(ByVal AtacanteIndex As Integer, ByVal VictimaIndex As Integer)
Dim ArmaObjInd As Integer, ObjInd As Integer
Dim num As Long

ArmaObjInd = UserList(AtacanteIndex).Invent.WeaponEqpObjIndex
ObjInd = 0

If ArmaObjInd > 0 Then
    If ObjData(ArmaObjInd).proyectil = 0 Then
        ObjInd = ArmaObjInd
    Else
        ObjInd = UserList(AtacanteIndex).Invent.MunicionEqpObjIndex
    End If
    
    If ObjInd > 0 Then
        If (ObjData(ObjInd).Envenena = 1) Then
            num = RandomNumber(1, 100)
            
            If num < 60 Then
                UserList(VictimaIndex).flags.Envenenado = 1
                Call SendData(ToIndex, VictimaIndex, 0, "||" & UserList(AtacanteIndex).Name & " te ha envenenado!!" & FONTTYPE_FIGHT)
                Call SendData(ToIndex, AtacanteIndex, 0, "||Has envenenado a " & UserList(VictimaIndex).Name & "!!" & FONTTYPE_FIGHT)
            End If
        End If
    End If
End If

End Sub
