
;Universal PT2 and PT3 player for ZX Spectrum and MSX
;(c)2004-2007 S.V.Bulba <vorobey@mail.khstu.ru>
;http://bulba.untergrund.net (http://bulba.at.kz)
; i8080 adaptation by Stanislav Yudin (CityAceE)

	device	zxspectrum48


;Universal PT2 and PT3 player for ZX Spectrum and MSX
;(c)2004-2007 S.V.Bulba <vorobey@mail.khstu.ru>
;http://bulba.untergrund.net (http://bulba.at.kz)

;Release number
Release EQU "1"

;Conditional assembly
;1) Version of ROUT (ZX or MSX standards)
ZX=0
MSX=0
SPEC=1
;2) Current position counter at (START+11)
CurPosCounter=0
;3) Allow channels allocation bits at (START+10)
ACBBAC=0
;4) Allow loop checking and disabling
LoopChecker=1
;5) Insert official identificator
Id=1

;Features
;--------
;-Can be compiled at any address (i.e. no need rounding ORG
; address).
;-Variables (VARS) can be located at any address (not only after
;code block).
;-INIT subprogram checks PT3-module version and rightly
; generates both note and volume tables outside of code block
; (in VARS).
;-Two portamento (spc. command 3xxx) algorithms (depending of
; PT3 module version).
;-New 1.XX and 2.XX special command behaviour (only for PT v3.7
; and higher).
;-Any Tempo value are accepted (including Tempo=1 and Tempo=2).
;-Fully compatible with Ay_Emul PT3 and PT2 players codes.
;-See also notes at the end of this source code.

;Limitations
;-----------
;-Can run in RAM only (self-modified code is used).
;-PT2 position list must be end by #FF marker only.

;Warning!!! PLAY subprogram can crash if no module are loaded
;into RAM or INIT subprogram was not called before.

;Call MUTE or INIT one more time to mute sound after stopping
;playing

	; ORG #C000

	ORG	0
startprog:

	jp	beginning	; При использовании прерываний

	include "macros.asm"

;----------------------------------------------
; Пример проигрывания мелодии для ПК Специалист
	ORG	56	; При использовании прерываний
	ret		; При использовании прерываний

beginning:
;Test codes (commented)

	LD A,3 ;PT2,ABC,NotLooped
	
	
	LD A,1 ;PT3,ABC,NotLooped

	LD (START+12),A
	CALL START
LOOP
	EI		; При использовании прерываний
	HALT		; При использовании прерываний

	; call wait	; При отсутствии прерываний

	CALL START+6
	; Без перехода на цикл
	LD A,(START+12)
	RLA
	JP NC,LOOP
	RET

; Простая задержка, дополняет код до 20 мс на один кадр
DELAY		equ	1022	; Приблизительная задержка для правильного темпа
wait:	ld	de, DELAY
wloop:	dec	de
	ld	a, d
	or	e
	jp	nz,  wloop
	ret
;----------------------------------------------


TonA	EQU 0
TonB	EQU 2
TonC	EQU 4
Noise	EQU 6
Mixer	EQU 7
AmplA	EQU 8
AmplB	EQU 9
AmplC	EQU 10
Env	EQU 11
EnvTp	EQU 13

;Z80
;Entry and other points
;START initialize playing of module at MDLADDR
;START+3 initialization with module address in HL
;START+5 play one quark
;START+8 mute
;START+10 setup and status flags
;START+11 current position value (byte) (optional)

;i8080
;Entry and other points
;START initialize playing of module at MDLADDR
;START+3 initialization with module address in HL
;START+6 play one quark
;START+9 mute
;START+12 setup and status flags
;START+13 current position value (byte) (optional)

START
	LD HL,MDLADDR
	JP INIT
	JP PLAY
	JP MUTE
SETUP	DB 0 ;set bit0, if you want to play without looping
	     ;(optional);
	     ;set bit1 for PT2 and reset for PT3 before
	     ;calling INIT;
	     ;bits2-3: %00-ABC, %01 ACB, %10 BAC (optional);
	     ;bits4-6 are not used
	     ;bit7 is set each time, when loop point is passed
	     ;(optional)
	IF CurPosCounter
CurPos	DB 0 ;for visualization only (i.e. no need for playing)
	ENDIF

;Identifier
	IF Id
	DB "=Uni PT2 and PT3 Player r.",Release,"="
	ENDIF

	IF LoopChecker
CHECKLP LD HL,SETUP

	; SET 7,(HL)		; OK
	ld	a, (hl)
	or	0b10000000
	ld	(hl), a

	; BIT 0,(HL)		; OK
	ld	a, (hl)
	and	0b1

	RET Z
	POP HL
	LD HL,DelyCnt
	INC (HL)
	LD HL,ChanA+CHP.NtSkCn
	INC (HL)
	ENDIF

MUTE	XOR A
	LD H,A
	LD L,A
	LD (AYREGS+AmplA),A
	LD (AYREGS+AmplB),HL
	JP ROUT

INIT
;HL - AddressOfModule

	; LD A,(START+10)		; Z80
	LD A,(START+12)

	AND 2
	JP NZ,INITPT2

	CALL SETMDAD
	PUSH HL
	LD DE,100
	ADD HL,DE
	LD A,(HL)
	LD (Delay),A
	PUSH HL

	; POP IX			; OK
	ex	(sp), hl
	ld	(rIX), hl
	pop	hl

	ADD HL,DE
	LD (CrPsPtr),HL

	; LD E,(IX+102-100)		; OK
	push	hl
	ld	hl, (rIX)
	ld	bc, 102-100
	add	hl, bc
	ld	e, (hl)
	pop	hl

	INC HL

	IF CurPosCounter
	LD A,L
	LD (PosSub+1),A
	ENDIF

	ADD HL,DE
	LD (LPosPtr),HL

	; LD L,(IX+103-100)		; OK
	; LD H,(IX+104-100)
	ld	hl, (rIX)
	ld	de, 103-100
	add	hl, de
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
	ex	de, hl

	POP DE

	ADD HL,DE
	LD (PatsPtr),HL
	LD HL,169
	ADD HL,DE
	LD (OrnPtrs),HL
	LD HL,105
	ADD HL,DE
	LD (SamPtrs),HL

	; LD A,(IX+13-100) ;EXTRACT VERSION NUMBER	; OK
	ld	hl, (rIX)
	ld	bc, 13-100
	add	hl, bc
	ld	a, (hl)

	SUB #30
	JP C,L20
	CP 10
	JP C,L21
L20	LD A,6
L21	LD (Version),A
	PUSH AF ;VolTable version
	CP 4

	; LD A,(IX+99-100) ;TONE TABLE NUMBER		; OK
	ld	hl, (rIX)
	ld	bc, 99-100
	add	hl, bc
	ld	a, (hl)

	RLA
	AND 7
	PUSH AF ;NoteTable number

	; LD HL,(e_-SamCnv-2)*256+#18		; OK
	; LD (SamCnv),HL
	ld	a, 0xc3 ; Формируем JP e_ вместо JR e_
	ld	(SamCnv), a
	ld	hl, e_
	ld	(SamCnv+1), hl

	LD A,#BA
	LD (OrnCP),A
	LD (SamCP),A
	LD A,#7B
	LD (OrnLD),A
	LD (SamLD),A
	LD A,#87
	LD (SamClc2),A
	LD BC,PT3PD
	LD HL,0
	LD DE,PT3EMPTYORN
	JP INITCOMMON

INITPT2 LD A,(HL)
	LD (Delay),A
	PUSH HL
	PUSH HL
	PUSH HL
	INC HL
	INC HL
	LD A,(HL)
	INC HL
	LD (SamPtrs),HL
	LD E,(HL)
	INC HL
	LD D,(HL)
	POP HL
	AND A

	; SBC HL,DE		; OK
	push	af
	ld	a, l
	sbc	a, e
	ld	l, a
	ld	a, h
	sbc	a, d
	ld	h, a
	pop	af

	CALL SETMDAD
	POP HL
	LD DE,67
	ADD HL,DE
	LD (OrnPtrs),HL
	LD E,32
	ADD HL,DE
	LD C,(HL)
	INC HL
	LD B,(HL)
	LD E,30
	ADD HL,DE
	LD (CrPsPtr),HL
	LD E,A
	INC HL

	IF CurPosCounter
	LD A,L
	LD (PosSub+1),A
	ENDIF

	ADD HL,DE
	LD (LPosPtr),HL
	POP HL
	ADD HL,BC
	LD (PatsPtr),HL
	LD A,5
	LD (Version),A
	PUSH AF
	LD A,2
	PUSH AF

	; LD HL,#51CB			; OK
	; LD (SamCnv),HL
	xor	a			; Формируем 3 NOP вместо BIT 2,C
	ld	(SamCnv), a
	ld	hl, 0
	ld	(SamCnv+1), hl

	LD A,#BB
	LD (OrnCP),A
	LD (SamCP),A
	LD A,#7A
	LD (OrnLD),A
	LD (SamLD),A
	LD A,#80
	LD (SamClc2),A
	LD BC,PT2PD
	LD HL,#8687
	LD DE,PT2EMPTYORN

INITCOMMON
	; LD (PTDECOD+1),BC		; OK
	push	hl
	ld	h, b
	ld	l, c
	ld	(PTDECOD+1), hl
	pop	hl

	LD (PsCalc),HL
	PUSH DE

;note table data depacker
;(c) Ivan Roshin
	LD DE,T_PACK
	LD BC,T1_+(2*49)-1
TP_0	LD A,(DE)
	INC DE
	CP 15*2
	JP NC,TP_1
	LD H,A
	LD A,(DE)
	LD L,A
	INC DE
	JP TP_2
TP_1	PUSH DE
	LD D,0
	LD E,A
	ADD HL,DE
	ADD HL,DE
	POP DE
TP_2	LD A,H
	LD (BC),A
	DEC BC
	LD A,L
	LD (BC),A
	DEC BC
	SUB #F8*2%256
	JP NZ,TP_0

	IF LoopChecker
	LD HL,SETUP

	; RES 7,(HL)		; OK
	push	af
	ld	a, (hl)
	and	0b01111111
	ld	(hl), a
	pop	af

	IF CurPosCounter
	INC HL
	LD (HL),A
	ENDIF

	ELSE
	IF CurPosCounter
	LD (CurPos),A
	ENDIF
	ENDIF

	LD HL,VARS
	LD (HL),A
	LD DE,VARS+1
	LD BC,VAR0END-VARS-1

	; LDIR			; OK
	push	af
1:	ld	a, (hl)
	ld	(de), a
	inc	hl
	inc	de
	dec	bc
	ld	a, b
	or	c
	jp	nz, 1b
	pop	af

	LD (AdInPtA),HL ;ptr to zero
	INC A
	LD (DelyCnt),A
	LD HL,#F001 ;H - CHP.Volume, L - CHP.NtSkCn
	LD (ChanA+CHP.NtSkCn),HL
	LD (ChanB+CHP.NtSkCn),HL
	LD (ChanC+CHP.NtSkCn),HL
	POP HL
	LD (ChanA+CHP.OrnPtr),HL
	LD (ChanB+CHP.OrnPtr),HL
	LD (ChanC+CHP.OrnPtr),HL

	POP AF

;NoteTableCreator (c) Ivan Roshin
;A - NoteTableNumber*2+VersionForNoteTable
;(xx1b - 3.xx..3.4r, xx0b - 3.4x..3.6x..VTII1.0)

	LD HL,NT_DATA
	PUSH DE
	LD D,B
	ADD A,A
	LD E,A
	ADD HL,DE
	LD E,(HL)
	INC HL

	; SRL E			; OK
	ld	(lab_29 + 1), a
	ld	a, e
	or	a
	rra
	ld	e, a
lab_29:	ld	a, 0

	SBC A,A
	AND #A7 ;#00 (NOP) or #A7 (AND A)
	LD (L3),A
	EX DE,HL
	POP BC ;BC=T1_
	ADD HL,BC

	LD A,(DE)
	ADD A,T_%256
	LD C,A
	ADC A,T_/256
	SUB C
	LD B,A
	PUSH BC
	LD DE,NT_
	PUSH DE

	LD B,12
L1	PUSH BC
	LD C,(HL)
	INC HL
	PUSH HL
	LD B,(HL)

	PUSH DE
	EX DE,HL
	LD DE,23

	; LD IXH,8			; OK
	ld	a, 8
	ld	(rIX + 1), a

L2
	; SRL B				; OK
	ld	a, b
	or	a
	rra
	ld	b, a

	; RR C				; OK
	ld	a, c
	rra
	ld	c, a

L3	DB #19	;AND A or NOP
	LD A,C
	ADC A,D ;=ADC 0
	LD (HL),A
	INC HL
	LD A,B
	ADC A,D
	LD (HL),A
	ADD HL,DE

	; DEC IXH			; OK
	push	hl
	ld	hl, (rIX)
	dec	h
	ld	(rIX), hl
	pop	hl

	JP NZ,L2

	POP DE
	INC DE
	INC DE
	POP HL
	INC HL
	POP BC

	; DJNZ L1			; OK
	dec	b
	jp	z, lab_01
	jp	L1
lab_01:

	POP HL
	POP DE

	LD A,E
	CP TCOLD_1%256
	JP NZ,CORR_1
	LD A,#FD
	LD (NT_+#2E),A

CORR_1	LD A,(DE)
	AND A
	JP Z,TC_EXIT
	RRA
	PUSH AF
	ADD A,A
	LD C,A
	ADD HL,BC
	POP AF
	JP NC,CORR_2
	DEC (HL)
	DEC (HL)
CORR_2	INC (HL)
	AND A

	; SBC HL,BC			; OK
	ld	a, l
	sbc	a, c
	ld	l, a
	ld	a, h
	sbc	a, b
	ld	h, a

	INC DE
	JP CORR_1

TC_EXIT

	POP AF

;VolTableCreator (c) Ivan Roshin
;A - VersionForVolumeTable (0..4 - 3.xx..3.4x;
			   ;5.. - 2.x,3.5x..3.6x..VTII1.0)

	CP 5
	LD HL,#11
	LD D,H
	LD E,H
	LD A,#17
	JP NC,M1
	DEC L
	LD E,L
	XOR A
M1	LD (M2),A

	; LD IX,VT_+16		; OK
	push	hl
	ld	hl, VT_+16
	ld	(rIX), hl
	pop	hl

	LD C,#F
INITV2	PUSH HL

	ADD HL,DE
	EX DE,HL

	; SBC HL,HL		; OK
	ld	hl,0
	
	LD B,#10
INITV1	LD A,L
M2	DB #7D
	LD A,H
	ADC A,0

	; LD (IX),A		; OK
	; INC IX
	push	hl
	ld	hl, (rIX)
	ld	(hl), a
	inc	hl
	ld	(rIX), hl
	pop	hl

	ADD HL,DE

	; DJNZ INITV1		; OK
	dec	b
	jp	z, lab_02
	jp	INITV1
lab_02:

	POP HL
	LD A,E
	CP #77
	JP NZ,M3
	INC E
M3	DEC C
	JP NZ,INITV2

	JP ROUT

SETMDAD LD (MODADDR),HL
	LD (MDADDR1),HL
	LD (MDADDR2),HL
	RET

PTDECOD JP #C3C3

;PT2 pattern decoder
PD2_SAM CALL SETSAM
	JP PD2_LOOP

PD2_EOff
	; LD (IX-12+CHP.Env_En),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.Env_En
	add	hl, de
	ld	(hl), a

	JP PD2_LOOP

PD2_ENV
	; LD (IX-12+CHP.Env_En),16	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.Env_En
	add	hl, de
	ld	(hl), 16

	LD (AYREGS+EnvTp),A
	LD A,(BC)
	INC BC
	LD L,A
	LD A,(BC)
	INC BC
	LD H,A
	LD (EnvBase),HL
	JP PD2_LOOP

PD2_ORN CALL SETORN
	JP PD2_LOOP

PD2_SKIP INC A

	; LD (IX-12+CHP.NNtSkp),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.NNtSkp
	add	hl, de
	ld	(hl), a

	JP PD2_LOOP

PD2_VOL RRCA
	RRCA
	RRCA
	RRCA

	; LD (IX-12+CHP.Volume),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.Volume
	add	hl, de
	ld	(hl), a

	JP PD2_LOOP

PD2_DEL CALL C_DELAY
	JP PD2_LOOP

PD2_GLIS
	; SET 2,(IX-12+CHP.Flags)	; OK
	push	af
	ld	hl, (rIX)
	ld	de, -12+CHP.Flags
	add	hl, de
	ld	a, (hl)
	or	0b100
	ld	(hl), a
	pop	af

	INC A

	; LD (IX-12+CHP.TnSlDl),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.TnSlDl
	add	hl, de
	ld	(hl), a

	; LD (IX-12+CHP.TSlCnt),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.TSlCnt
	add	hl, de
	ld	(hl), a

	LD A,(BC)
	INC BC

	; LD (IX-12+CHP.TSlStp),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.TSlStp
	add	hl, de
	ld	(hl), a

	ADD A,A
	SBC A,A

	; LD (IX-12+CHP.TSlStp+1),A	; OK
	inc	hl
	ld	(hl), a

	SCF
	JP PD2_LP2

PT2PD	AND A

PD2_LP2
	; EX AF,AF'		; OK
	push	af
	ld 	hl, (rAF1)
	ex 	(sp), hl
	ld 	(rAF1), hl
	pop 	af

PD2_LOOP LD A,(BC)
	INC BC
	ADD A,#20
	JP Z,PD2_REL
	JP C,PD2_SAM
	ADD A,96
	JP C,PD2_NOTE
	INC A
	JP Z,PD2_EOff
	ADD A,15
	JP Z,PD_FIN
	JP C,PD2_ENV
	ADD A,#10
	JP C,PD2_ORN
	ADD A,#40
	JP C,PD2_SKIP
	ADD A,#10
	JP C,PD2_VOL
	INC A
	JP Z,PD2_DEL
	INC A
	JP Z,PD2_GLIS
	INC A
	JP Z,PD2_PORT
	INC A
	JP Z,PD2_STOP
	LD A,(BC)
	INC BC

	; LD (IX-12+CHP.CrNsSl),A	; Не удалось проверить, но 99%, что работает
	ld	hl, (rIX)
	ld	de, -12+CHP.CrNsSl
	add	hl, de
	ld	(hl), a

	JP PD2_LOOP

PD2_PORT
	; RES 2,(IX-12+CHP.Flags)	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.Flags
	add	hl, de
	ld	a, (hl)
	and	0b11111011
	ld	(hl), a

	LD A,(BC)
	INC BC
	INC BC ;ignoring precalc delta to right sound
	INC BC
	SCF
	JP PD2_LP2

PD2_STOP
	; LD (IX-12+CHP.TSlCnt),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.TSlCnt
	add	hl, de
	ld	(hl), a

	JP PD2_LOOP

PD2_REL
	; LD (IX-12+CHP.Flags),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.Flags
	add	hl, de
	ld	(hl), A

	JP PD2_EXIT

PD2_NOTE LD L,A

	; LD A,(IX-12+CHP.Note)		; OK
	push	hl
	ld	hl, (rIX)
	ld	de, -12+CHP.Note
	add	hl, de
	ld	a, (hl)
	pop	hl

	LD (PrNote+1),A

	; LD (IX-12+CHP.Note),L		; OK
	ld	a, l
	ld	hl, (rIX)
	ld	de, -12+CHP.Note
	add	hl, de
	ld	(hl), a

	XOR A

	; LD (IX-12+CHP.TSlCnt),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.TSlCnt
	add	hl, de
	ld	(hl), a

	; SET 0,(IX-12+CHP.Flags)	; OK
	push	af
	ld	hl, (rIX)
	ld	de, -12+CHP.Flags
	add	hl, de
	ld	a, (hl)
	or	0b1
	ld	(hl), a
	pop	af

	; EX AF,AF'		; OK
	push	af
	ld 	hl, (rAF1)
	ex 	(sp), hl
	ld 	(rAF1), hl
	pop 	af

	JP NC,NOGLIS2

	; BIT 2,(IX-12+CHP.Flags)	; OK
	ld	(lab_03 + 1), a
	ld	hl, (rIX)
	ld	de, -12+CHP.Flags
	add	hl, de
	ld	a, (hl)
	and	0b100
lab_03:	ld	a, 0

	JP NZ,NOPORT2
	LD (LoStep),A
	ADD A,A
	SBC A,A

	; EX AF,AF'		; OK
	push	af
	ld 	hl, (rAF1)
	ex 	(sp), hl
	ld 	(rAF1), hl
	pop 	af

	LD H,A
	LD L,A
	INC A
	CALL SETPORT
NOPORT2
	; LD (IX-12+CHP.TSlCnt),1	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.TSlCnt
	add	hl, de
	ld	(hl), 1

NOGLIS2 XOR A


PD2_EXIT
	; LD (IX-12+CHP.PsInSm),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.PsInSm
	add	hl, de
	ld	(hl), a

	; LD (IX-12+CHP.PsInOr),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.PsInOr
	add	hl, de
	ld	(hl), a

	; LD (IX-12+CHP.CrTnSl),A	; OK
	; LD (IX-12+CHP.CrTnSl+1),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.CrTnSl
	add	hl, de
	ld	(hl), a
	inc	hl
	ld	(hl), a

	JP PD_FIN

;PT3 pattern decoder
PD_OrSm
	; LD (IX-12+CHP.Env_En),0	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.Env_En
	add	hl, de
	ld	(hl), 0

	CALL SETORN
PD_SAM_ LD A,(BC)
	INC BC
	RRCA

PD_SAM	CALL SETSAM
	JP PD_LOOP

PD_VOL	RRCA
	RRCA
	RRCA
	RRCA

	; LD (IX-12+CHP.Volume),A	; OK
	push	de
	ld	hl, (rIX)
	ld	de, -12+CHP.Volume
	add	hl, de
	ld	(hl), a
	pop	de

	JP PD_LP2

PD_EOff
	; LD (IX-12+CHP.Env_En),A	; OK
	; LD (IX-12+CHP.PsInOr),A	; OK
	push	de
	ld	hl, (rIX)
	ld	de, -12+CHP.Env_En
	add	hl, de
	ld	(hl), a
	ld	hl, (rIX)
	ld	de, -12+CHP.PsInOr
	add	hl, de
	ld	(hl), a
	pop	de

	JP PD_LP2

PD_SorE DEC A
	JP NZ,PD_ENV
	LD A,(BC)
	INC BC

	; LD (IX-12+CHP.NNtSkp),A	; OK
	push	de
	ld	hl, (rIX)
	ld	de, -12+CHP.NNtSkp
	add	hl, de
	ld	(hl), a
	pop	de

	JP PD_LP2

PD_ENV	CALL SETENV
	JP PD_LP2

PD_ORN	CALL SETORN
	JP PD_LOOP

PD_ESAM
	; LD (IX-12+CHP.Env_En),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.Env_En
	add	hl, de
	ld	(hl), a

	; LD (IX-12+CHP.PsInOr),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.PsInOr
	add	hl, de
	ld	(hl), a

	CALL NZ,SETENV
	JP PD_SAM_

PT3PD
	; LD A,(IX-12+CHP.Note)		; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.Note
	add	hl, de
	ld	a, (hl)

	LD (PrNote+1),a

	; LD L,(IX-12+CHP.CrTnSl)	; OK
	; LD H,(IX-12+CHP.CrTnSl+1)
	ld	hl, (rIX)
	ld	de, -12+CHP.CrTnSl
	add	hl, de
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
	ex	de, hl

	LD (PrSlide+1),HL

PD_LOOP LD DE,#2010
PD_LP2	LD A,(BC)
	INC BC
	ADD A,E
	JP C,PD_OrSm
	ADD A,D
	JP Z,PD_FIN
	JP C,PD_SAM
	ADD A,E
	JP Z,PD_REL
	JP C,PD_VOL
	ADD A,E
	JP Z,PD_EOff
	JP C,PD_SorE
	ADD A,96
	JP C,PD_NOTE
	ADD A,E
	JP C,PD_ORN
	ADD A,D
	JP C,PD_NOIS
	ADD A,E
	JP C,PD_ESAM
	ADD A,A
	LD E,A
	LD HL,SPCCOMS+#FF20-#2000
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	PUSH DE
	JP PD_LOOP

PD_NOIS LD (Ns_Base),A
	JP PD_LP2

PD_REL
	; RES 0,(IX-12+CHP.Flags)	; OK
	push	af
	ld	hl, (rIX)
	ld	de, -12+CHP.Flags
	add	hl, de
	ld	a, (hl)
	and	0b11111110
	ld	(hl), a
	pop	af

	JP PD_RES

PD_NOTE
	; LD (IX-12+CHP.Note),A		; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.Note
	add	hl, de
	ld	(hl), a

	; SET 0,(IX-12+CHP.Flags)	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.Flags
	add	hl, de
	ld	a, (hl)
	or	0b1
	ld	(hl), a

	XOR A

PD_RES
	; LD (PDSP_+1),SP		; OK
	ld	hl, 0x0000
	add	hl, sp
	ld	(PDSP_+1), hl

	; LD SP,IX			; OK
	ld	hl, (rIX)
	ld	sp, hl

	LD H,A
	LD L,A
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
PDSP_	LD SP,#3131

PD_FIN
	; LD A,(IX-12+CHP.NNtSkp)	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.NNtSkp
	add	hl, de
	ld	a, (hl)

	; LD (IX-12+CHP.NtSkCn),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.NtSkCn
	add	hl, de
	ld	(hl), a

	RET

C_PORTM LD A,(BC)
	INC BC
;SKIP PRECALCULATED TONE DELTA (BECAUSE
;CANNOT BE RIGHT AFTER PT3 COMPILATION)
	INC BC
	INC BC

	; EX AF,AF'			; OK
	push	af
	ld 	hl, (rAF1)
	ex 	(sp), hl
	ld 	(rAF1), hl
	pop 	af

	LD A,(BC) ;SIGNED TONE STEP
	INC BC
	LD (LoStep),A
	LD A,(BC)
	INC BC
	AND A

	; EX AF,AF'			; OK
	push	af
	ld 	hl, (rAF1)
	ex 	(sp), hl
	ld 	(rAF1), hl
	pop 	af

	; LD L,(IX-12+CHP.CrTnSl)	; OK
	; LD H,(IX-12+CHP.CrTnSl+1)
	push	af
	ld	hl, (rIX)
	ld	de, -12+CHP.CrTnSl
	add	hl, de
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
	ex	de, hl
	pop	af

;Set portamento variables
;A - Delay; A' - Hi(Step); ZF' - (A'=0); HL - CrTnSl

SETPORT
	; RES 2,(IX-12+CHP.Flags)	; OK
	; LD (IX-12+CHP.TnSlDl),A	; OK
	; LD (IX-12+CHP.TSlCnt),A	; OK
	push	hl
	push	af
	ld	hl, (rIX)
	ld	de, -12+CHP.Flags
	add	hl, de
	ld	a, (hl)
	and	0b11111011
	ld	(hl), a
	pop	af
	ld	hl, (rIX)
	ld	de, -12+CHP.TnSlDl
	add	hl, de
	ld	(hl), a
	ld	hl, (rIX)
	ld	de, -12+CHP.TSlCnt
	add	hl, de
	ld	(hl), a
	pop	hl

	PUSH HL
	LD DE,NT_

	; LD A,(IX-12+CHP.Note)		; OK
	; LD (IX-12+CHP.SlToNt),A	; OK
	push	de
	ld	hl, (rIX)
	ld	de, -12+CHP.Note
	add	hl, de
	ld	a, (hl)
	ld	hl, (rIX)
	ld	de, -12+CHP.SlToNt
	add	hl, de
	ld	(hl), a
	pop	de

	ADD A,A
	LD L,A
	LD H,0
	ADD HL,DE
	LD A,(HL)
	INC HL
	LD H,(HL)
	LD L,A
	PUSH HL
PrNote	LD A,#3E

	; LD (IX-12+CHP.Note),A		; OK
	push	de
	ld	hl, (rIX)
	ld	de, -12+CHP.Note
	add	hl, de
	ld	(hl), a
	pop	de

	ADD A,A
	LD L,A
	LD H,0
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	POP HL

	; SBC HL,DE			; OK
	ld	a, l
	sbc	a, e
	ld	l, a
	ld	a, h
	sbc	a, d
	ld	h, a

	; LD (IX-12+CHP.TnDelt),L	; OK
	; LD (IX-12+CHP.TnDelt+1),H	; OK
	push	bc
	ex	de, hl
	ld	hl, (rIX)
	ld	bc, -12+CHP.TnDelt
	add	hl, bc
	ld	(hl), e
	inc	hl
	ld	(hl), d
	pop	bc

	POP DE
Version EQU $+1
	LD A,#3E
	CP 6
	JP C,OLDPRTM ;Old 3xxx for PT v3.5-
PrSlide LD DE,#1111

	; LD (IX-12+CHP.CrTnSl),E	; OK
	; LD (IX-12+CHP.CrTnSl+1),D	; OK
	push	bc
	ld	hl, (rIX)
	ld	bc, -12+CHP.CrTnSl
	add	hl, bc
	ld	(hl), e
	inc	hl
	ld	(hl), d
	pop	bc

LoStep	EQU $+1
OLDPRTM LD A,#3E

	; EX AF,AF'		; OK
	push	af
	ld 	hl, (rAF1)
	ex 	(sp), hl
	ld 	(rAF1), hl
	pop 	af

	JP Z,NOSIG
	EX DE,HL
NOSIG
	; SBC HL,DE		; OK
	ld	(lab_04 + 1), a
	ld	a, l
	sbc	a, e
	ld	l, a
	ld	a, h
	sbc	a, d
	ld	h, a
lab_04:	ld	a, 0

	JP P,SET_STP
	CPL

	; EX AF,AF'		; OK
	push	af
	ld 	hl, (rAF1)
	ex 	(sp), hl
	ld 	(rAF1), hl
	pop 	af

	; NEG		; OK
	cpl
	add 0x01

	; EX AF,AF'	; OK
	push	af
	ld 	hl, (rAF1)
	ex 	(sp), hl
	ld 	(rAF1), hl
	pop 	af

SET_STP
	; LD (IX-12+CHP.TSlStp+1),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.TSlStp+1
	add	hl, de
	ld	(hl), a

	; EX AF,AF'	; OK
	push	af
	ld 	hl, (rAF1)
	ex 	(sp), hl
	ld 	(rAF1), hl
	pop 	af

	; LD (IX-12+CHP.TSlStp),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.TSlStp
	add	hl, de
	ld	(hl), a

	; LD (IX-12+CHP.COnOff),0	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.COnOff
	add	hl, de
	ld	(hl), 0

	RET

C_GLISS
	; SET 2,(IX-12+CHP.Flags)	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.Flags
	add	hl, de
	ld	a, (hl)
	or	0b100
	ld	(hl), a

	LD A,(BC)
	INC BC

	; LD (IX-12+CHP.TnSlDl),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.TnSlDl
	add	hl, de
	ld	(hl), a

	AND A
	JP NZ,GL36
	LD A,(Version) ;AlCo PT3.7+
	CP 7
	SBC A,A
	INC A
GL36
	; LD (IX-12+CHP.TSlCnt),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.TSlCnt
	add	hl, de
	ld	(hl), a

	LD A,(BC)
	INC BC

	; EX AF,AF'		; OK
	push	af
	ld 	hl, (rAF1)
	ex 	(sp), hl
	ld 	(rAF1), hl
	pop 	af	

	LD A,(BC)
	INC BC
	JP SET_STP

C_SMPOS LD A,(BC)
	INC BC

	; LD (IX-12+CHP.PsInSm),A	; Не смог проверить. Вероятность правильной работы - 99%
	ld	hl, (rIX)
	ld	de, -12+CHP.PsInSm
	add	hl, de
	ld	(hl), a

	RET

C_ORPOS LD A,(BC)
	INC BC
	
	; LD (IX-12+CHP.PsInOr),A	; Не смог проверить. Вероятность правильной работы - 99%
	ld	hl, (rIX)
	ld	de, -12+CHP.PsInOr
	add	hl, de
	ld	(hl), a

	RET

C_VIBRT LD A,(BC)
	INC BC

	; LD (IX-12+CHP.OnOffD),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.OnOffD
	add	hl, de
	ld	(hl), a

	; LD (IX-12+CHP.COnOff),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.COnOff
	add	hl, de
	ld	(hl), A

	LD A,(BC)
	INC BC

	; LD (IX-12+CHP.OffOnD),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.OffOnD
	add	hl, de
	ld	(hl), a

	XOR A

	; LD (IX-12+CHP.TSlCnt),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.TSlCnt
	add	hl, de
	ld	(hl), a

	; LD (IX-12+CHP.CrTnSl),A	; OK
	; LD (IX-12+CHP.CrTnSl+1),A	; OK
	ld	hl, (rIX)
	ld	de, -12+CHP.CrTnSl
	add	hl, de
	ld	(hl), a
	inc	hl
	ld	(hl), a

	RET

C_ENGLS LD A,(BC)
	INC BC
	LD (Env_Del),A
	LD (CurEDel),A
	LD A,(BC)
	INC BC
	LD L,A
	LD A,(BC)
	INC BC
	LD H,A
	LD (ESldAdd),HL
	RET

C_DELAY LD A,(BC)
	INC BC
	LD (Delay),A
	RET

SETENV
	; LD (IX-12+CHP.Env_En),E	; OK
	push	bc
	ld	hl, (rIX)
	ld	bc, -12+CHP.Env_En
	add	hl, bc
	ld	(hl), e
	pop	bc

	LD (AYREGS+EnvTp),A
	LD A,(BC)
	INC BC
	LD H,A
	LD A,(BC)
	INC BC
	LD L,A
	LD (EnvBase),HL
	XOR A

	; LD (IX-12+CHP.PsInOr),A	; OK
	push	de
	ld	hl, (rIX)
	ld	de, -12+CHP.PsInOr
	add	hl, de
	ld	(hl), a
	pop	de

	LD (CurEDel),A
	LD H,A
	LD L,A
	LD (CurESld),HL
C_NOP	RET

SETORN	ADD A,A
	LD E,A
	LD D,0

	; LD (IX-12+CHP.PsInOr),D	; OK
	push	bc
	ld	hl, (rIX)
	ld	bc, -12+CHP.PsInOr
	add	hl, bc
	ld	(hl), d
	pop	bc
	

OrnPtrs EQU $+1
	LD HL,#2121
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
MDADDR2 EQU $+1
	LD HL,#2121
	ADD HL,DE

	; LD (IX-12+CHP.OrnPtr),L	; OK
	; LD (IX-12+CHP.OrnPtr+1),H	; OK
	push	bc
	ex	de, hl
	ld	hl, (rIX)
	ld	bc, -12+CHP.OrnPtr
	add	hl, bc
	ld	(hl), e
	inc	hl
	ld	(hl), d
	pop	bc

	RET

SETSAM	ADD A,A
	LD E,A
	LD D,0
SamPtrs EQU $+1
	LD HL,#2121
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
MDADDR1 EQU $+1
	LD HL,#2121
	ADD HL,DE

	; LD (IX-12+CHP.SamPtr),L	; OK
	; LD (IX-12+CHP.SamPtr+1),H	; OK
	push	bc
	ex	de, hl
	ld	hl, (rIX)
	ld	bc, -12+CHP.SamPtr
	add	hl, bc
	ld	(hl), e
	inc	hl
	ld	(hl), d
	pop	bc

	RET

;ALL 16 ADDRESSES TO PROTECT FROM BROKEN PT3 MODULES
SPCCOMS DW C_NOP
	DW C_GLISS
	DW C_PORTM
	DW C_SMPOS
	DW C_ORPOS
	DW C_VIBRT
	DW C_NOP
	DW C_NOP
	DW C_ENGLS
	DW C_DELAY
	DW C_NOP
	DW C_NOP
	DW C_NOP
	DW C_NOP
	DW C_NOP
	DW C_NOP

CHREGS	XOR A
	LD (Ampl),A

	; BIT 0,(IX+CHP.Flags)		; OK
	push	hl
	ld	hl, (rIX)
	ld	de, CHP.Flags
	add	hl, de
	ld	(lab_05 + 1), a
	ld	a, (hl)
	and	0b1
lab_05:	ld	a, 0
	pop	hl

	PUSH HL
	JP Z,CH_EXIT

	; С ЭТОГО МЕСТА НЕЛЬЗЯ ИСПОЛЬЗОВАТЬ СТЕК

	; LD (CSP_+1),SP		; OK
	ld	hl, 0x0000
	add	hl, sp
	ld	(CSP_+1), hl

	; LD L,(IX+CHP.OrnPtr)		; OK
	; LD H,(IX+CHP.OrnPtr+1)
	ld	(lab_24 + 1), a
	ld	hl, (rIX)
	ld	de, CHP.OrnPtr
	add	hl, de
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
	ex	de, hl
lab_24:	ld	a, 0

	LD SP,HL
	POP DE
	LD H,A

	; LD A,(IX+CHP.PsInOr)		; OK
	ld	(lab_25 + 1), hl
	ld	hl, (rIX)
	ld	bc, CHP.PsInOr
	add	hl, bc
	ld	a, (hl)
lab_25:	ld	hl, 0

	LD L,A
	ADD HL,SP
	INC A
		;PT2	PT3
OrnCP	INC A	;CP E	CP D
	JP C,CH_ORPS
OrnLD	DB 1	;LD A,D LD A,E
CH_ORPS
	; LD (IX+CHP.PsInOr),A	; OK
	ld	(lab_26 + 1), hl
	ld	hl, (rIX)
	ld	de, CHP.PsInOr
	add	hl, de
	ld	(hl), a
lab_26:	ld	hl, 0

	; LD A,(IX+CHP.Note)	; OK
	ld	(lab_27 + 1), hl
	ld	hl, (rIX)
	ld	de, CHP.Note
	add	hl, de
	ld	a, (hl)
lab_27:	ld	hl, 0

	ADD A,(HL)
	JP P,CH_NTP
	XOR A
CH_NTP	CP 96
	JP C,CH_NOK
	LD A,95
CH_NOK	ADD A,A

	; EX AF,AF'		; OK
	ld	(rAF1), a	; NO SP Нет возможности сохранить F

	; LD L,(IX+CHP.SamPtr)	; OK
	; LD H,(IX+CHP.SamPtr+1)
	ld	hl, (rIX)
	ld	de, CHP.SamPtr
	add	hl, de
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
	ex	de, hl

	LD SP,HL
	POP DE

	; LD A,(IX+CHP.PsInSm)	; OK
	ld	hl, (rIX)
	ld	bc, CHP.PsInSm
	add	hl, bc
	ld	a, (hl)

	LD H,0

	LD B,A
	ADD A,A
SamClc2 ADD A,A ;or ADD A,B for PT2
	LD L,A
	ADD HL,SP
	LD SP,HL
	LD A,B
	INC A
		;PT2	PT3
SamCP	INC A	;CP E	CP D
	JP C,CH_SMPS
SamLD	DB 1	;LD A,D LD A,E
CH_SMPS
	; LD (IX+CHP.PsInSm),A		; OK
	ld	hl, (rIX)
	ld	bc, CHP.PsInSm
	add	hl, bc
	ld	(hl), a

	POP BC
	POP HL

;Convert PT2 sample to PT3
		; PT2		PT3
SamCnv	POP HL	; BIT 2,C	JR e_
	POP HL

;====================================
; Проблемное место c самомодификацией SamCnv
; В оригинальном коде на место двух предыдущих POP HL ставится либо BIT 2,C, либо JR e_
; И та и другая команда отсутствуют у i8080, плюс эти команды двухбайтовые
	nop	; Дополнение до третьего байта в команде
	ld	(lab_06 + 1), a
	ld	a, c
	and	0b100
lab_06:	ld	a, 0
;====================================

	LD H,B

	; JP NZ,$+8
	jp	nz, lab_07

	EX DE,HL
	AND A

	; SBC HL,HL		; OK
	ld	hl, 0

	; SBC HL,DE		; OK
	ld	a, l
	sbc	a, e
	ld	l, a
	ld	a, h
	sbc	a, d
	ld	h, a

lab_07:
	LD D,C

	; RR C			; OK
	ld	a, c
	rra
	ld	c, a

	SBC A,A
	CPL
	AND #3E

	; RR C			; OK
	; RR B			; OK
	ld	(lab_08 + 1), a
	ld	a, c
	rra
	ld	c, a
	ld	a, b
	rra
	ld	b, a
lab_08:	ld	a, 0

	AND C
	LD C,A
	LD A,B
	RRA
	RRA

	; RR D			; OK
	ld	(lab_10 + 1), a
	ld	a, d
	rra
	ld	d, a
lab_10:	ld	a, 0

	RRA
	AND #9F
	LD B,A

e_
	; LD E,(IX+CHP.TnAcc)	; OK
	; LD D,(IX+CHP.TnAcc+1) 
	ld	(lab_28 + 1), hl
	ld	hl, (rIX)
	ld	de, CHP.TnAcc
	add	hl, de
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
lab_28:	ld	hl, 0

	ADD HL,DE

	; BIT 6,B		; OK
	ld	a, 0b1000000
	and	b

	JP Z,CH_NOAC

	; LD (IX+CHP.TnAcc),L		; OK NO SP
	; LD (IX+CHP.TnAcc+1),H		; OK NO SP
	ld	(lab_12 + 1), hl
	ex	de, hl
	ld	h, b
	ld	l, c
	ld	(lab_11 + 1), hl
	ld	hl, (rIX)
	ld	bc, CHP.TnAcc
	add	hl, bc
	ld	(hl), e
	inc	hl
	ld	(hl), d
	ex	de, hl
lab_11:	ld	bc, 0
lab_12:	ld	hl, 0

CH_NOAC EX DE,HL

	; EX AF,AF'		; OK
	ld	hl, (rAF1)	; NO SP Нет возможности сохранить F
	ld	(rAF1), a
	ld	a, l

	ADD A,NT_%256
	LD L,A
	ADC A,NT_/256
	SUB L
	LD H,A
	LD SP,HL
	POP HL
	ADD HL,DE

	; LD E,(IX+CHP.CrTnSl)		; OK
	; LD D,(IX+CHP.CrTnSl+1)
	ld	(lab_13 + 1), hl
	ld	hl, (rIX)
	ld	de, CHP.CrTnSl
	add	hl, de
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
lab_13:	ld	hl, 0

	ADD HL,DE
CSP_	LD SP,#3131
	EX (SP),HL
	XOR A

	; С ЭТОГО МЕСТА МОЖНО ИСПОЛЬЗОВАТЬ СТЕК

	; OR (IX+CHP.TSlCnt)	; OK
	push	de
	ld	hl, (rIX)
	ld	de, CHP.TSlCnt
	add	hl, de
	or	(hl)
	pop	de

	JP Z,CH_AMP

	; DEC (IX+CHP.TSlCnt)	; OK
	push	de
	ld	hl, (rIX)
	ld	de, CHP.TSlCnt
	add	hl, de
	dec	(hl)
	pop	de

	JP NZ,CH_AMP

	; LD A,(IX+CHP.TnSlDl)		; OK
	push	de
	ld	hl, (rIX)
	ld	de, CHP.TnSlDl
	add	hl, de
	ld	a, (hl)
	pop	de

	; LD (IX+CHP.TSlCnt),A		; OK
	push	de
	ld	hl, (rIX)
	ld	de, CHP.TSlCnt
	add	hl, de
	ld	(hl), a
	pop	de

	; LD L,(IX+CHP.TSlStp)		; OK
	; LD H,(IX+CHP.TSlStp+1)
	push	de
	ld	hl, (rIX)
	ld	de, CHP.TSlStp
	add	hl, de
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
	ex	de, hl
	pop	de
	
	LD A,H
	ADD HL,DE

	; LD (IX+CHP.CrTnSl),L		; OK
	; LD (IX+CHP.CrTnSl+1),H	; OK
	push	bc
	ex	de, hl
	ld	hl, (rIX)
	ld	bc, CHP.CrTnSl
	add	hl, bc
	ld	(hl), e
	inc	hl
	ld	(hl), d
	ex	de, hl
	pop	bc

	; BIT 2,(IX+CHP.Flags)		; OK
	push	hl
	ld	(lab_14 + 1), a
	ld	hl, (rIX)
	ld	de, CHP.Flags
	add	hl, de
	ld	a, (hl)
	and	0b100
lab_14:	ld	a, 0
	pop	hl

	JP NZ,CH_AMP

	; LD E,(IX+CHP.TnDelt)		; OK
	; LD D,(IX+CHP.TnDelt+1) 	; OK
	push	af
	push	hl
	ld	hl, (rIX)
	ld	de, CHP.TnDelt
	add	hl, de
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
	pop	hl
	pop	af

	AND A
	JP Z,CH_STPP
	EX DE,HL
CH_STPP
	; SBC HL,DE			; OK
	ld	a, l
	sbc	a, e
	ld	l, a
	ld	a, h
	sbc	a, d
	ld	h, a

	JP M,CH_AMP

	; LD A,(IX+CHP.SlToNt)		; OK
	ld	hl, (rIX)
	ld	de, CHP.SlToNt
	add	hl, de
	ld	a, (hl)

	; LD (IX+CHP.Note),A		; OK
	ld	hl, (rIX)
	ld	de, CHP.Note
	add	hl, de
	ld	(hl), a

	XOR A

	; LD (IX+CHP.TSlCnt),A		; OK
	ld	hl, (rIX)
	ld	de, CHP.TSlCnt
	add	hl, de
	ld	(hl), a

	; LD (IX+CHP.CrTnSl),A		; OK
	; LD (IX+CHP.CrTnSl+1),A	; OK
	ld	hl, (rIX)
	ld	de, CHP.CrTnSl
	add	hl, de
	ld	(hl), a
	inc	hl
	ld	(hl), a

CH_AMP
	; LD A,(IX+CHP.CrAmSl)		; OK
	ld	hl, (rIX)
	ld	de, CHP.CrAmSl
	add	hl, de
	ld	a, (hl)

	; BIT 7,C			; OK
	ld	(lab_15 + 1), a
	ld	a, 0b10000000
	and	c
lab_15:	ld	a, 0

	JP Z,CH_NOAM

	; BIT 6,C			; OK
	ld	(lab_16 + 1), a
	ld	a, 0b01000000
	and	c
lab_16:	ld	a, 0

	JP Z,CH_AMIN
	CP 15
	JP Z,CH_NOAM
	INC A
	JP CH_SVAM
CH_AMIN CP -15
	JP Z,CH_NOAM
	DEC A
CH_SVAM
	; LD (IX+CHP.CrAmSl),A		; OK
	ld	hl, (rIX)
	ld	de, CHP.CrAmSl
	add	hl, de
	ld	(hl), a

CH_NOAM LD L,A
	LD A,B
	AND 15
	ADD A,L
	JP P,CH_APOS
	XOR A
CH_APOS CP 16
	JP C,CH_VOL
	LD A,15
CH_VOL
	; OR (IX+CHP.Volume)		; OK
	ld	hl, (rIX)
	ld	de, CHP.Volume
	add	hl, de
	or	(hl)

	ADD A,VT_%256
	LD L,A
	ADC A,VT_/256
	SUB L
	LD H,A
	LD A,(HL)
CH_ENV
	; BIT 0,C			; OK
	ld	(lab_17 + 1), a
	ld	a, 0b00000001
	and	c
lab_17:	ld	a, 0

	JP NZ,CH_NOEN

	; OR (IX+CHP.Env_En)		; OK
	ld	hl, (rIX)
	ld	de, CHP.Env_En
	add	hl, de
	or	(hl)

CH_NOEN LD (Ampl),A

	; BIT 7,B			; OK
	ld	a, 0b10000000
	and	b

	LD A,C
	JP Z,NO_ENSL
	RLA
	RLA

	; SRA A				; OK
	ld	(lab_18 + 1), a
	rla
lab_18:	ld	a, 0
	rra

	; SRA A				; OK
	ld	(lab_19 + 1), a
	rla
lab_19:	ld	a, 0
	rra

	; SRA A				; OK
	ld	(lab_20 + 1), a
	rla
lab_20:	ld	a, 0
	rra

	; ADD A,(IX+CHP.CrEnSl) ;SEE COMMENT BELOW	; OK
	ld	hl, (rIX)
	ld	de, CHP.CrEnSl
	add	hl, de
	add	(hl)

	; BIT 5,B			; OK
	ld	(lab_21 + 1), a
	ld	a, 0b00100000
	and	b
lab_21:	ld	a, 0

	JP Z,NO_ENAC

	; LD (IX+CHP.CrEnSl),A		; OK
	ld	hl, (rIX)
	ld	de, CHP.CrEnSl
	add	hl, de
	ld	(hl), a

NO_ENAC LD HL,AddToEn
	ADD A,(HL) ;BUG IN PT3 - NEED WORD HERE
	LD (HL),A
	JP CH_MIX
NO_ENSL RRA

	; ADD A,(IX+CHP.CrNsSl)		; OK
	ld	hl, (rIX)
	ld	de, CHP.CrNsSl
	add	hl, de
	add	(hl)

	LD (AddToNs),A

	; BIT 5,B			; OK
	ld	(lab_22 + 1), a
	ld	a, 0b00100000
	and	b
lab_22:	ld	a, 0

	JP Z,CH_MIX

	; LD (IX+CHP.CrNsSl),A		; OK
	ld	hl, (rIX)
	ld	de, CHP.CrNsSl
	add	hl, de
	ld	(hl), a

CH_MIX	LD A,B
	RRA
	AND #48
CH_EXIT LD HL,AYREGS+Mixer
	OR (HL)
	RRCA
	LD (HL),A
	POP HL
	XOR A

	; OR (IX+CHP.COnOff)		; OK
	push	hl		; без этого точно глючит!
	ld	hl, (rIX)
	ld	de, CHP.COnOff
	add	hl, de
	or	(hl)
	pop	hl		; без этого точно глючит!

	RET Z

	; DEC (IX+CHP.COnOff)		; OK
	ld	hl, (rIX)
	ld	de, CHP.COnOff
	add	hl, de
	dec	(hl)

	RET NZ

	; XOR (IX+CHP.Flags)		; OK
	ld	hl, (rIX)
	ld	de, CHP.Flags
	add	hl, de
	xor	(hl)

	; LD (IX+CHP.Flags),A		; OK
	ld	hl, (rIX)
	ld	de, CHP.Flags
	add	hl, de
	ld	(hl), a

	RRA

	; LD A,(IX+CHP.OnOffD)		; OK
	push	af
	ld	hl, (rIX)
	ld	de, CHP.OnOffD
	add	hl, de
	pop	af
	ld	a, (hl)

	JP C,CH_ONDL

	; LD A,(IX+CHP.OffOnD)		; OK
	ld	hl, (rIX)
	ld	de, CHP.OffOnD
	add	hl, de
	ld	a, (hl)

CH_ONDL
	; LD (IX+CHP.COnOff),A		; OK
	ld	hl, (rIX)
	ld	de, CHP.COnOff
	add	hl, de
	ld	(hl), a

	RET

PLAY	XOR A
	LD (AddToEn),A
	LD (AYREGS+Mixer),A
	DEC A
	LD (AYREGS+EnvTp),A
	LD HL,DelyCnt
	DEC (HL)
	JP NZ,PL2
	LD HL,ChanA+CHP.NtSkCn
	DEC (HL)
	JP NZ,PL1B
AdInPtA EQU $+1
	LD BC,#0101
	LD A,(BC)
	AND A
	JP NZ,PL1A
	LD D,A
	LD (Ns_Base),A
CrPsPtr EQU $+1
	LD HL,#2121
	INC HL
	LD A,(HL)
	INC A
	JP NZ,PLNLP

	IF LoopChecker
	CALL CHECKLP
	ENDIF

LPosPtr EQU $+1
	LD HL,#2121
	LD A,(HL)
	INC A
PLNLP	LD (CrPsPtr),HL
	DEC A
		;PT2		PT3
PsCalc	DEC A	;ADD A,A	NOP
	DEC A	;ADD A,(HL)	NOP
	ADD A,A
	LD E,A

	; RL D			; OK
	ld	a, d
	rla
	ld	d, a

	IF CurPosCounter
	LD A,L
PosSub	SUB #D6
	LD (CurPos),A
	ENDIF

PatsPtr EQU $+1
	LD HL,#2121
	ADD HL,DE
MODADDR EQU $+1
	LD DE,#1111

	; LD (PSP_+1),SP	; OK
	ld	(lab_23 + 1), hl
	ld	hl, 0x0000
	add	hl, sp
	ld	(PSP_+1), hl
lab_23:	ld	hl, 0

	LD SP,HL
	POP HL
	ADD HL,DE
	LD B,H
	LD C,L
	POP HL
	ADD HL,DE
	LD (AdInPtB),HL
	POP HL
	ADD HL,DE
	LD (AdInPtC),HL
PSP_	LD SP,#3131
PL1A
	; LD IX,ChanA+12	; OK
	ld	hl, ChanA+12
	ld	(rIX), hl

	CALL PTDECOD

	; LD (AdInPtA),BC	; OK
	ld	h, b
	ld	l, c
	ld	(AdInPtA), hl

PL1B	LD HL,ChanB+CHP.NtSkCn
	DEC (HL)
	JP NZ,PL1C

	; LD IX,ChanB+12	; OK
	ld	hl, ChanB+12
	ld	(rIX), hl

AdInPtB EQU $+1
	LD BC,#0101
	CALL PTDECOD

	; LD (AdInPtB),BC	; OK
	ld	h, b
	ld	l, c
	ld	(AdInPtB), hl

PL1C	LD HL,ChanC+CHP.NtSkCn
	DEC (HL)
	JP NZ,PL1D

	; LD IX,ChanC+12	; OK
	ld	hl, ChanC+12
	ld	(rIX), hl

AdInPtC EQU $+1
	LD BC,#0101
	CALL PTDECOD

	; LD (AdInPtC),BC	; OK
	ld	h, b
	ld	l, c
	ld	(AdInPtC), hl

Delay	EQU $+1
PL1D	LD A,#3E
	LD (DelyCnt),A

PL2
	; LD IX,ChanA		; OK
	ld	hl, ChanA
	ld	(rIX), hl

	LD HL,(AYREGS+TonA)
	CALL CHREGS
	LD (AYREGS+TonA),HL
	LD A,(Ampl)
	LD (AYREGS+AmplA),A

	; LD IX,ChanB		; OK
	ld	hl, ChanB
	ld	(rIX), hl

	LD HL,(AYREGS+TonB)
	CALL CHREGS
	LD (AYREGS+TonB),HL
	LD A,(Ampl)
	LD (AYREGS+AmplB),A

	; LD IX,ChanC		; OK
	ld	hl, ChanC
	ld	(rIX), hl

	LD HL,(AYREGS+TonC)
	CALL CHREGS
	LD (AYREGS+TonC),HL

	LD HL,(Ns_Base_AddToNs)
	LD A,H
	ADD A,L
	LD (AYREGS+Noise),A

AddToEn EQU $+1
	LD A,#3E
	LD E,A
	ADD A,A
	SBC A,A
	LD D,A
	LD HL,(EnvBase)
	ADD HL,DE

	; LD DE,(CurESld)	; OK
	ex	de, hl
	ld	hl, (CurESld)
	ex	de, hl

	ADD HL,DE
	LD (AYREGS+Env),HL

	XOR A
	LD HL,CurEDel
	OR (HL)
	JP Z,ROUT
	DEC (HL)
	JP NZ,ROUT
Env_Del EQU $+1
	LD A,#3E
	LD (HL),A
ESldAdd EQU $+1
	LD HL,#2121
	ADD HL,DE
	LD (CurESld),HL

ROUT
	IF ACBBAC
	LD A,(SETUP)
	AND 12
	JP Z,ABC
	ADD A,CHTABLE%256
	LD E,A
	ADC A,CHTABLE/256
	SUB E
	LD D,A
	LD B,0
	; LD IX,AYREGS
	LD HL,AYREGS
	LD A,(DE)
	INC DE
	LD C,A
	ADD HL,BC
	; LD A,(IX+TonB)
	LD C,(HL)
	; LD (IX+TonB),C
	LD (HL),A
	INC HL
	; LD A,(IX+TonB+1)
	LD C,(HL)
	; LD (IX+TonB+1),C
	LD (HL),A
	LD A,(DE)
	INC DE
	LD C,A
	ADD HL,BC
	; LD A,(IX+AmplB)
	LD C,(HL)
	; LD (IX+AmplB),C
	LD (HL),A
	LD A,(DE)
	INC DE
	LD (RxCA1),A
	XOR 8
	LD (RxCA2),A
	LD HL,AYREGS+Mixer
	LD A,(DE)
	AND (HL)
	LD E,A
	LD A,(HL)
RxCA1	LD A,(HL)
	AND %010010
	OR E
	LD E,A
	LD A,(HL)
	AND %010010
RxCA2	OR E
	OR E
	LD (HL),A
ABC
	ENDIF

	IF ZX
	XOR A
	LD DE,#FFBF
	LD BC,#FFFD
	LD HL,AYREGS
LOUT	OUT (C),A
	LD B,E
	OUTI
	LD B,D
	INC A
	CP 13
	JP NZ,LOUT
	OUT (C),A
	LD A,(HL)
	AND A
	RET M
	LD B,E
	OUT (C),A
	RET
	ENDIF

	IF MSX
;MSX version of ROUT (c)Dioniso
	XOR A
	LD C,#A0
	LD HL,AYREGS
LOUT	OUT (C),A
	INC C
	OUTI
	DEC C
	INC A
	CP 13
	JP NZ,LOUT
	OUT (C),A
	LD A,(HL)
	AND A
	RET M
	INC C
	OUT (C),A
	RET
	ENDIF

	IF SPEC
AY_SEL	equ    0xf403	 ; Выбор регистра AY
AY_WR	equ    0xf401	 ; Запись в регистр AY
AY_RD	equ    0xf400	 ; Чтение из регистра AY

;Специалист
	xor	a
	ld	hl, AYREGS	; HL = указатель на данные AY
	ld	c, 0x0D		; C = 13 (счётчик регистров)
LOUT:
	ld	(AY_SEL), a	; Выбор регистра AY (0xF403)
	ld	b, a
	ld	a, (hl)		; Читаем значение в A
	ld	(AY_WR), a	; Записываем в AY (0xF401)
	ld	a, b
	inc	hl		; Переходим к следующему значению
	inc	a		; Увеличиваем номер регистра (A++)
	cp	c		; Проверяем, достигли ли 13 (C=0x0D)
	jp	nz, LOUT	; Если нет — повторяем
	ld	(AY_SEL), a	; Выбираем регистр 13
	ld	a, (hl)		; Читаем его значение
	and	a		; Проверяем бит 7 (для RET M)
	ret	m		; Если бит 7 = 1 (знак минус), возвращаемся
	ld	(AY_WR), a	; Иначе записываем значение регистра 13
	ret
	ENDIF

	IF ACBBAC
CHTABLE EQU $-4
	DB 4,5,15,%001001,0,7,7,%100100
	ENDIF

NT_DATA DB (T_NEW_0-T1_)*2
	DB TCNEW_0-T_
	DB (T_OLD_0-T1_)*2+1
	DB TCOLD_0-T_
	DB (T_NEW_1-T1_)*2+1
	DB TCNEW_1-T_
	DB (T_OLD_1-T1_)*2+1
	DB TCOLD_1-T_
	DB (T_NEW_2-T1_)*2
	DB TCNEW_2-T_
	DB (T_OLD_2-T1_)*2
	DB TCOLD_2-T_
	DB (T_NEW_3-T1_)*2
	DB TCNEW_3-T_
	DB (T_OLD_3-T1_)*2
	DB TCOLD_3-T_

T_

TCOLD_0 DB #00+1,#04+1,#08+1,#0A+1,#0C+1,#0E+1,#12+1,#14+1
	DB #18+1,#24+1,#3C+1,0
TCOLD_1 DB #5C+1,0
TCOLD_2 DB #30+1,#36+1,#4C+1,#52+1,#5E+1,#70+1,#82,#8C,#9C
	DB #9E,#A0,#A6,#A8,#AA,#AC,#AE,#AE,0
TCNEW_3 DB #56+1
TCOLD_3 DB #1E+1,#22+1,#24+1,#28+1,#2C+1,#2E+1,#32+1,#BE+1,0
TCNEW_0 DB #1C+1,#20+1,#22+1,#26+1,#2A+1,#2C+1,#30+1,#54+1
	DB #BC+1,#BE+1,0
TCNEW_1 EQU TCOLD_1
TCNEW_2 DB #1A+1,#20+1,#24+1,#28+1,#2A+1,#3A+1,#4C+1,#5E+1
	DB #BA+1,#BC+1,#BE+1,0

PT3EMPTYORN EQU $-1
	DB 1,0

;first 12 values of tone tables (packed)

T_PACK	DB #06EC*2/256,#06EC*2%256
	DB #0755-#06EC
	DB #07C5-#0755
	DB #083B-#07C5
	DB #08B8-#083B
	DB #093D-#08B8
	DB #09CA-#093D
	DB #0A5F-#09CA
	DB #0AFC-#0A5F
	DB #0BA4-#0AFC
	DB #0C55-#0BA4
	DB #0D10-#0C55
	DB #066D*2/256,#066D*2%256
	DB #06CF-#066D
	DB #0737-#06CF
	DB #07A4-#0737
	DB #0819-#07A4
	DB #0894-#0819
	DB #0917-#0894
	DB #09A1-#0917
	DB #0A33-#09A1
	DB #0ACF-#0A33
	DB #0B73-#0ACF
	DB #0C22-#0B73
	DB #0CDA-#0C22
	DB #0704*2/256,#0704*2%256
	DB #076E-#0704
	DB #07E0-#076E
	DB #0858-#07E0
	DB #08D6-#0858
	DB #095C-#08D6
	DB #09EC-#095C
	DB #0A82-#09EC
	DB #0B22-#0A82
	DB #0BCC-#0B22
	DB #0C80-#0BCC
	DB #0D3E-#0C80
	DB #07E0*2/256,#07E0*2%256
	DB #0858-#07E0
	DB #08E0-#0858
	DB #0960-#08E0
	DB #09F0-#0960
	DB #0A88-#09F0
	DB #0B28-#0A88
	DB #0BD8-#0B28
	DB #0C80-#0BD8
	DB #0D60-#0C80
	DB #0E10-#0D60
	DB #0EF8-#0E10

;vars from here can be stripped
;you can move VARS to any other address

VARS

;ChannelsVars
	STRUCT	CHP
;reset group
PsInOr	DB 0
PsInSm	DB 0
CrAmSl	DB 0
CrNsSl	DB 0
CrEnSl	DB 0
TSlCnt	DB 0
CrTnSl	DW 0
TnAcc	DW 0
COnOff	DB 0
;reset group
OnOffD	DB 0
;IX for PTDECOD here (+12)
OffOnD	DB 0
OrnPtr	DW 0
SamPtr	DW 0
NNtSkp	DB 0
Note	DB 0
SlToNt	DB 0
Env_En	DB 0
Flags	DB 0
 ;Enabled - 0,SimpleGliss - 2
TnSlDl	DB 0
TSlStp	DW 0
TnDelt	DW 0
NtSkCn	DB 0
Volume	DB 0
	ENDS

ChanA	DS CHP
ChanB	DS CHP
ChanC	DS CHP

;GlobalVars
DelyCnt DB 0
CurESld DW 0
CurEDel DB 0
Ns_Base_AddToNs
Ns_Base DB 0
AddToNs DB 0

AYREGS

VT_	DS 256 ;CreatedVolumeTableAddress

EnvBase EQU VT_+14

T1_	EQU VT_+16 ;Tone tables data depacked here

T_OLD_1 EQU T1_
T_OLD_2 EQU T_OLD_1+24
T_OLD_3 EQU T_OLD_2+24
T_OLD_0 EQU T_OLD_3+2
T_NEW_0 EQU T_OLD_0
T_NEW_1 EQU T_OLD_1
T_NEW_2 EQU T_NEW_0+24
T_NEW_3 EQU T_OLD_3

PT2EMPTYORN EQU VT_+31 ;1,0,0 sequence

NT_	DS 192 ;CreatedNoteTableAddress

;local var
Ampl	EQU AYREGS+AmplC

VAR0END EQU VT_+16 ;INIT zeroes from VARS to VAR0END-1

VARSEND EQU $

MDLADDR EQU $

;Release 0 steps:
;02/27/2005
;Merging PT2 and PT3 players; debug
;02/28/2005
;debug; optimization
;03/01/2005
;Migration to SjASM; conditional assembly (ZX, MSX and
;visualization)
;03/03/2005
;SETPORT subprogram (35 bytes shorter)
;03/05/2005
;fixed CurPosCounter error
;03/06/2005
;Added ACB and BAC channels swapper (for Spectre); more cond.
;assembly keys; optimization
;Release 1 steps:
;04/15/2005
;Removed loop bit resetting for no loop build (5 bytes shorter)
;04/30/2007
;New 1.xx and 2.xx interpretation for PT 3.7+.

;Tests in IMMATION TESTER V1.0 by Andy Man/POS
;(for minimal build)
;Module name/author	Min tacts	Max tacts
;PT3 (a little slower than standalone player)
;Spleen/Nik-O		1720		9368
;Chuta/Miguel		1720		9656
;Zhara/Macros		4536		8792
;PT2 (more slower than standalone player)
;Epilogue/Nik-O		3928		10232
;NY tHEMEs/zHenYa	3848		9208
;GUEST 4/Alex Job	2824		9352
;KickDB/Fatal Snipe	1720		9880

;Size (minimal build for ZX Spectrum):
;Code block #7B9 bytes
;Variables #21D bytes (can be stripped)
;Size in RAM #7B9+#21D=#9D6 (2518) bytes

;Notes:
;Pro Tracker 3.4r can not be detected by header, so PT3.4r tone
;tables realy used only for modules of 3.3 and older versions.

	; incbin	"Music/Slash - Molodoy (1996).pt2"
	; incbin	"Music/DNK - CALL ME (1998).pt2"
	; incbin	"Music/DNK - WATER (1998).pt2"

	; incbin	"Music/nq - GABBA (2019).pt3"
	; incbin	"Music/Ishma - Love (1999).pt3"
	; incbin	"Music/Karbofos - Zima, Moroz & pol-litra (2006) (Forever 7, 6).pt3"
	; incbin	"Music/aGGreSSor - Our Summer (2021) (Chaos Constructions Summer 2021, 6).pt3"
	incbin	"Music/Davos - 'Sannikov theme (1999).pt3"
	; incbin 	"Music/VAN - KAMUSHEK (2000).pt3"
	
endprog:
	savebin "PTxPlay.bin", startprog, endprog - startprog

rAF1:	dw	0	; Временное хранение AF'
rIX:	dw	0	; Аналог регистра IX
