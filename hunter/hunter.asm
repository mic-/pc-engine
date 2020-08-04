; Hunter : An NSF player for the HuC6280
; /Mic, 2010, 2012
; 
; Assemble with wla-dx
;
; Memory map:
;
; $0000-$1FFF:  RAM
; $2000-$3FFF:  RAM
; $4000-$5FFF:  RAM
; $6000-$7FFF:  I/O / RAM
; $8000-$9FFF:  NSF part 1
; $A000-$BFFF:  NSF part 2
; $C000-$DFFF:  NSF part 3
; $E000-$FFFF:  Playback code / NSF part 4


.memorymap
    defaultslot 0
    slotsize $2000
    slot 0 $0000
    slot 1 $2000
    slot 2 $4000
    slot 3 $6000
    slot 4 $8000
    slot 5 $A000
    slot 6 $C000
    slot 7 $E000
.endme

.rombankmap
    bankstotal 8
    banksize $2000
    banks 8
.endro


.define IO_BASE         $6000

.define VDC_CTRL        IO_BASE+$0000
.define VDC_DATA_L      IO_BASE+$0002
.define VDC_DATA_H      IO_BASE+$0003

.define VCE_CTRL        IO_BASE+$0400
.define VCE_INDEX_L     IO_BASE+$0402
.define VCE_INDEX_H     IO_BASE+$0403
.define VCE_DATA_L      IO_BASE+$0404
.define VCE_DATA_H      IO_BASE+$0405

.define PSG_CHANNEL_SELECT  IO_BASE+$0800
.define PSG_GLOBAL_BALANCE  IO_BASE+$0801
.define PSG_FREQ_FINE       IO_BASE+$0802
.define PSG_FREQ_COARSE     IO_BASE+$0803
.define PSG_CHANNEL_CTRL    IO_BASE+$0804
.define PSG_CHANNEL_BALANCE IO_BASE+$0805
.define PSG_CHANNEL_WAVE    IO_BASE+$0806
.define PSG_NOISE_CTRL      IO_BASE+$0807
.define PSG_LFO_FREQ        IO_BASE+$0808
.define PSG_LFO_CTRL        IO_BASE+$0809

.define TIMER_COUNTER       IO_BASE+$0C00
.define TIMER_CTRL          IO_BASE+$0C01

.DEFINE GAMEPAD_IO          IO_BASE+$1000

.define INTERRUPT_CTRL      IO_BASE+$1402
.define INTERRUPT_STATUS    IO_BASE+$1403

.define HEADER_LOCATION     $F800

.define SONG_LOCATION       $8000

.define RAM_CODE_BASE       $3E00


.enum $0010
indPtr     ds 2
.ende

.enum $3C00
initAddress ds 2
playAddress ds 2
jumpAddress ds 2
currentSong ds 1
currentNSF  ds 1
nsfSpeed    ds 2     ; 3C08
nsfMode     ds 1
waitTimer   ds 1
delay       ds 2
tempw       ds 2
apuRegs     ds 32    ; 3C10
chn0Ctrl    ds 1     ; 3C30
chn2Ctrl    ds 1
chn3Ctrl    ds 1
chn4Ctrl    ds 1
chnCtrlPrev ds 4     ; previous values for chn*Ctrl
chnEnable   ds 4
temp        ds 1
.ende

.enum $3D00
samples           ds 8
chnEnv            ds 4     ; channel envelopes
chnEnvCounter     ds 4
chnLen            ds 4     ; channel length counters
frameCounter      ds 1
linearCounter     ds 1     ; linear counter for the triangle channel
lcReloadFlag      ds 1     ; linear counter reload flag
invalidValue      ds 1
chnEnableOverride ds 4     ; used for forcing channels off when their period reaches zero
joyData           ds 1
keyState          ds 1
vramAddr          ds 2
textColor         ds 1
count1            ds 1
f0backup          ds 16
chnSweepCounter   ds 2
highlightedNSF    ds 1
highlightedSong   ds 1
currMaxSong       ds 1
.ende

.enum $3D80
sq1Per         ds 2     ; square 1 period
sq2Per         ds 2     ; square 2 period
sq1Swp         ds 1     ; square 1 sweep settings
sq2Swp         ds 1     ; square 2 sweep settings
chnOverdrive   ds 4     ; channel overdrives (boost square/noise volume)
color          ds 4
updateSongInfo ds 1
speedOverride  ds 1
apuStatus      ds 1
apuStatus2     ds 1
channelMap     ds 4
freeChannel    ds 1
fcMode         ds 1     ; frame counter mode
playCount      ds 1
triPerHi       ds 1     ; triangle period MSB
.ende


.macro INCW
    inc     <\1
    bne     +
    inc     <\1+1
    +:
.endm

; Ex: sti16 someZeroPageVar,1234
.macro sti16
    lda     #<\2
    sta     \1
    lda     #>\2
    sta     \1+1
.endm


; Ex: addi16 someZeroPageVar,1234
.macro addi16
    lda     \1
    clc
    adc     #<\2
    sta     \1
    lda     \1+1
    adc     #>\2
    sta     \1+1
.endm


; Set the VDC Memory Address Write Register
.macro set_mawri
    st0     #0    ; select VDC register 0 (MAWR)
    st1     #<\1
    st2	    #>\1
.endm


.org $0000
.dw 6280

.org $17FC
.db 1        ; number of songs. this will be overwritten by the rombuilder

.org $1800
.incbin "header.bin"


; Interrupt vectors
.org $1FF6

 .dw irq2+RAM_CODE_BASE
 .dw vdc_irq+RAM_CODE_BASE
 .dw timer_irq+RAM_CODE_BASE
 .dw irq2+RAM_CODE_BASE
 .dw start


.bank 0 slot 7
.org $0000
.section "text7"



start:
    sei
    csh              ; switch the CPU to high speed mode   
    cld
    ldx     #$FF
    txs

    lda     #$03     ; $0000-$1FFF: RAM
    tam     #$01
    lda     #$F8     ; $2000-$3FFF: RAM
    tam     #$02
    lda     #$F8     ; $4000-$5FFF: RAM
    tam     #$04
    lda     #$FF     ; $6000-$7FFF: I/O
    tam     #$08
    lda     #$02     ; $8000-$9FFF: ROM bank 2 (graphics data)
    tam     #$10
    lda     #$04     ; $A000-$BFFF: ROM bank 4 (NSF)
    tam     #$20
    lda     #$05     ; $C000-$DFFF: ROM bank 5 (NSF cont.)
    tam     #$40
    lda     #$00     ; $E000-$FFFF: ROM bank 0 (playback code)
    tam     #$80
    
    tii     ram_code_start,RAM_CODE_BASE,ram_code_end-ram_code_start
    
    lda     #$F8
    tam     #$01
    
    jsr     setup_video
    
    stz     currentNSF
    stz     highlightedNSF
    stz     currentSong
    stz     highlightedSong
    stz     speedOverride
    stz     waitTimer
    stz     delay
    stz     delay+1

    stz     joyData
    stz     keyState

    lda     #$FF
    sta     PSG_GLOBAL_BALANCE
    lda     #$00
    sta     PSG_LFO_CTRL     ; LFO off
    sta     PSG_LFO_FREQ

    lda     #<square_wave_12_5
    sta     <indPtr
    lda     #>square_wave_12_5
    sta     <indPtr+1

    ; Setup channel 0 (Square1)
    lda     #0
    sta     channelMap+0
    sta     PSG_CHANNEL_SELECT
    jsr     load_waveform
    lda     #$FF
    sta     PSG_CHANNEL_BALANCE

    ; Disable channel 1
    lda     #1
    sta     freeChannel
    sta     PSG_CHANNEL_SELECT
    stz     PSG_CHANNEL_CTRL
    lda     #$FF
    sta     PSG_CHANNEL_BALANCE
    
    ; Setup channel 2 (Square2)
    lda     #2
    sta     channelMap+1
    sta     PSG_CHANNEL_SELECT
    jsr     load_waveform
    lda     #$FF
    sta     PSG_CHANNEL_BALANCE	

    ; Setup channel 4 (Noise)
    lda     #4
    sta     channelMap+3
    sta     PSG_CHANNEL_SELECT
    jsr     load_waveform
    lda     #$CC
    sta     PSG_CHANNEL_BALANCE	

    ; Setup channel 3 (Triangle)
    lda     #<triangle_wave
    sta     <indPtr
    lda     #>triangle_wave
    sta     <indPtr+1
    lda     #3
    sta     channelMap+2
    sta     PSG_CHANNEL_SELECT
    jsr     load_waveform
    lda     #$FF
    sta     PSG_CHANNEL_BALANCE	

    ; Disable channel 5
    lda     #5
    sta     PSG_CHANNEL_SELECT
    stz     PSG_CHANNEL_CTRL

    lda     #0
    sta     currentSong

    lda     #4
    sta     frameCounter
    sta     playCount
    
    stz     triPerHi
    
    lda     #$FE
    sta     invalidValue

    lda     #0
    sta     chnOverdrive
    sta     chnOverdrive+1
    sta     chnOverdrive+3
    
    jsr     print_song_list
    lda     #$C3
    sta     vramAddr
    stz     vramAddr+1
    lda     #$11
    sta     textColor
    sti16   indPtr,song_num
    jsr     print_string
    jsr     print_current_nsf
    jsr     print_song_num
    
    ; Enable vblank IRQs (just so we can poll the vblank status in VDC_CTRL - no VDC interrupts
    ; will occur since they are disabled in INTERRUPT_CTRL).
    st0     #5
    st1     #$88
    st2     #0
    
    ; Display the triangle and noise waveform "icons" above the volume indicators
    set_mawri $698
    st0     #2
    st1     #100
    st2     #$31
    set_mawri $69B
    st0     #2
    st1     #101
    st2     #$31
  
    stz     fcMode
    stz     updateSongInfo
   
do_load:
    jmp     load_nsf

play:
   ; Wait until the previous delay is over
    lda     waitTimer
    bne     play

    ; Restart the timer. We want it to time out at 240 Hz (NTSC) or 200 Hz (PAL).
    lda     #29        ; NTSC delay: 6992/240 = 29
    sta     delay
    lda     nsfMode
    eor     speedOverride
    and     #1
    beq     +
    lda     #35        ; PAL delay: 6992/200 = 35
    sta     delay
+:
    jsr     start_timer

    lda     frameCounter
    cmp     #1
    bne     +
    lda     fcMode
    beq     +
    ; We're at the last step of a 5-step cycle; skip everything except
    ; updating frameCounter and playCount
    bra     update_counters
+:

    ; Write the HuC6280 channel control bytes as necessary
    clx
-:
    lda.w   chn0Ctrl,x
    cmp.w   chnCtrlPrev,x
    beq     +
    stx     temp
    ldy     channelMap,x
    sty     PSG_CHANNEL_SELECT
    cpy     #3     ; no overdrive for the triangle channel
    beq     ++
    sta     tempw
    and     #$1F
    clc
    adc.w   chnOverdrive,x
    cmp     #$20
    bcc     +++
    lda     #$1F
+++:
    sta     tempw+1
    lda     tempw
    and     #$E0
    ora     tempw+1
++:
    and.w   chnEnableOverride,x
    sta     PSG_CHANNEL_CTRL
+:
    inx
    cpx     #4
    bne     -
    
    ; Copy current state -> previous state, to be used during the next frame
    tii     $4000,apuRegs,$20
    tii     chn0Ctrl,chnCtrlPrev,4
    
    ; Envelopes and the linear counter are updated at every step of the frameCounter
    ; sequence, except at the last step of a 5-step sequence
    jsr     update_envelope
    jsr     update_linear_counter
    
    lda     frameCounter
    and     #1
    bne     +
    jsr     update_sweep    ; update the square waves' sweep units at steps 1 and 3 (or 0 and 2)
    jsr     update_length   ; update the length counters at steps 1 and 3 (or 0 and 2)
+:
update_counters:
    dec     frameCounter
    bne     +
    lda     #4
    clc
    adc     fcMode
    sta     frameCounter
+:
    dec     playCount
    beq     +
    jmp     play
+:
    lda     #4     ; the PLAY routine should be called at 240/4 = 60 Hz (50 for PAL)
    sta     playCount
    
    ; Try to detect writes to some of the NES APU registers by first writing some bogus value to them
    ; and then checking if they have changed after the call to the NSF's PLAY routine.
    lda     invalidValue
    sta     <$4003     ; square 1 length counter reload value / high period
    sta     <$4007     ; square 2 length counter reload value / high period
    sta     <$400B
    sta     <$400F     ; noise length counter reload value / envelope restart
    lda     #$7D
    sta     <$4001     ; square 1 sweep settings
    sta     <$4005     ; square 2 sweep settings
    
    lda     #17
    sta     <$4017
    
    lda     <$4015
    sta     apuStatus2

    ; Call the NSF's PLAY routine
    lda     playAddress
    sta     jumpAddress
    lda     playAddress+1
    sta     jumpAddress+1
    jsr     jsr_nsf+RAM_CODE_BASE

    cli     ; make sure that interrupts are enabled (in case the PLAY routine turned them off for some reason)

    lda     <$4017
    cmp     #17
    beq     +
    ; The frame counter register has been written to
    rol     a
    rol     a
    and     #1
    sta     fcMode     ; 0 for a 4-step sequence, 1 for a 5-step sequence
    clc
    adc     #4
    sta     frameCounter
+:

    ; Update volume, duty cycle and period as necessary for the 2A03 channels
    jsr     update_volume
    jsr     update_duty
    jsr     update_period

    jsr     read_joypad
    lda     keyState
    bne     check_keys_released
    lda     joyData
    sta     keyState
    jmp     no_new_keypress
check_keys_released:
    lda     joyData
    beq     keys_released
    jmp     no_new_keypress
keys_released:
    lda     keyState
    stz     keyState
    cmp     #$02     ; left
    bne     +
    dec     highlightedSong
    bpl     ++
    lda     currMaxSong
    dea
    sta     highlightedSong
++:
    lda     #1
    sta     updateSongInfo
    jmp     play 

+:
    cmp     #$01     ; right
    bne     +
    inc     highlightedSong
    lda     highlightedSong
    cmp     currMaxSong
    bcc     ++
    stz     highlightedSong
++:
    lda     #1
    sta     updateSongInfo
    jmp     play 

+:
    cmp     #$04     ; down
    bne     +
    inc     highlightedNSF
    lda     highlightedNSF
    cmp     HEADER_LOCATION-4
    bcc     +++
    stz     highlightedNSF
+++:
    lda     #1
    sta     updateSongInfo
    jmp     play 

+:
    cmp     #$08     ; up
    bne     +
    dec     highlightedNSF
    bpl     +++
    lda     HEADER_LOCATION-4
    dea
    sta     highlightedNSF
+++:
    lda     #1
    sta     updateSongInfo
    jmp     play 

+:
    cmp     #$40     ; ii
    bne     +
    lda     highlightedNSF
    cmp     currentNSF
    bne     ++
    lda     highlightedSong
    cmp     currentSong
    beq     +
    lda     highlightedSong
    sta     currentSong
    bra     +++
++:
    lda     highlightedNSF
    sta     currentNSF
    lda     #$FF
    sta     currentSong
    sta     highlightedSong
+++:
    lda     #1
    sta     updateSongInfo
    jmp     do_load

+:
    cmp     #$80     ; i
    bne     +
    jmp     do_load

+:
    cmp     #$20     ; select
    bne     +
    lda     chnOverdrive
    eor     #4
    sta     chnOverdrive
    sta     chnOverdrive+1
    sta     chnOverdrive+3

+:
    cmp     #$10     ; run
    bne     +
    lda     speedOverride
    eor     #1
    sta     speedOverride

+:
no_new_keypress:
    jmp     play


load_nsf:
    sei
    ; Clear RAM at $0000-$07FF
    stz     $2000
    tii     $2000,$2001,$07FF

    stz     apuRegs
    tii     apuRegs,apuRegs+1,$1F

    ; Reset the HuC6280 audio channels
    lda     channelMap+0 
    sta.w   PSG_CHANNEL_SELECT
    cla
    sta.w   PSG_CHANNEL_CTRL
    sta.w   PSG_FREQ_FINE
    sta.w   PSG_FREQ_COARSE

    lda     channelMap+1 
    sta.w   PSG_CHANNEL_SELECT
    cla
    sta.w   PSG_CHANNEL_CTRL
    sta.w   PSG_FREQ_FINE
    sta.w   PSG_FREQ_COARSE

    lda     channelMap+2 
    sta.w   PSG_CHANNEL_SELECT
    cla
    sta.w   PSG_CHANNEL_CTRL
    sta.w   PSG_FREQ_FINE
    sta.w   PSG_FREQ_COARSE

    lda     channelMap+3 
    sta.w   PSG_CHANNEL_SELECT
    cla
    sta.w   PSG_CHANNEL_CTRL
    sta.w   PSG_FREQ_FINE
    sta.w   PSG_FREQ_COARSE
    sta.w   PSG_NOISE_CTRL

    stz     chnCtrlPrev
    stz     chnCtrlPrev+1
    stz     chnCtrlPrev+2
    stz     chnCtrlPrev+3

    stz     chn0Ctrl
    stz     chn2Ctrl
    stz     chn3Ctrl
    stz     chn4Ctrl

    stz     sq1Per
    stz     sq1Per+1
    stz     sq2Per
    stz     sq2Per+1

    stz     sq1Swp
    stz     sq2Swp

    stz     apuStatus
    stz     apuStatus2

    lda     #$00
    sta     chnEnable
    sta     chnEnable+1
    sta     chnEnable+2
    sta     chnEnable+3
    lda     #$7F
    sta     chnEnableOverride
    sta     chnEnableOverride+1
    sta     chnEnableOverride+2
    lda     #$FF
    sta     chnEnableOverride+3
    lda     #$0F
    sta     $4015

    stz     chnLen
    stz     chnLen+1
    stz     chnLen+2
    stz     chnLen+3
    stz     linearCounter
    stz     lcReloadFlag

    lda     #1
    sta     chnSweepCounter
    sta     chnSweepCounter+1

    lda     #$0 ;F
    sta     chnEnv
    sta     chnEnv+1
    sta     chnEnv+2
    sta     chnEnv+3

    lda     currentNSF
    jsr     calc_header_offset
    lda     <indPtr
    clc
    adc     #<HEADER_LOCATION
    sta     <indPtr
    lda     <indPtr+1
    adc     #>HEADER_LOCATION
    sta     <indPtr+1

    lda     currentNSF
    asl     a
    asl     a
    ina
    ina
    ina
    ina
    sta     temp
    tam     #$10
    inc     temp
    lda     temp
    tam     #$20
    inc     temp
    lda     temp
    tam     #$40
    
    ldy     #$0A
    lda     (<indPtr),y
    sta     initAddress
    iny
    lda     (<indPtr),y
    sta     initAddress+1
    iny
    lda     (<indPtr),y
    sta     playAddress
    iny
    lda     (<indPtr),y
    sta     playAddress+1
    
    lda     currentSong
    cmp     #$FF
    bne     +
    ldy     #7
    lda     (<indPtr),y
    dea
    sta     currentSong
    sta     highlightedSong
+:

    ldy     #$7A
    lda     (<indPtr),y
    sta     nsfMode
load_song:
    stz     indPtr
    stz     indPtr+1
    ; Call the NSF's INIT routine
    lda     initAddress
    sta     jumpAddress
    lda     initAddress+1
    sta     jumpAddress+1
    lda     currentSong
    jsr     jsr_nsf+RAM_CODE_BASE 
    jsr     update_length
    jsr     update_linear_counter
    cli
    jmp     play


; Set indPtr = a*128
calc_header_offset:
    pha
    lsr     a
    ror     a
    and     #$80
    sta     <indPtr
    pla
    lsr     a
    sta     <indPtr+1
    rts


setup_video:
    lda     #1
    sta.w   VCE_CTRL

    ; Setup the VDC registers (352x240 resolution, 64x32 BAT, BG enabled)
    cly
-:
    lda.w   vdc_reg_table,y
    sta.w   VDC_CTRL
    iny
    lda.w   vdc_reg_table,y
    sta.w   VDC_DATA_L
    iny
    lda.w   vdc_reg_table,y
    sta.w   VDC_DATA_H
    iny
    cpy     #33
    bcc     -

    ; Clear the BAT
    set_mawri 0
    st0     #2
    ldy     #40
-:
    ldx     #64
--:
    st1     #$80
    st2     #0
    dex
    bne     --
    dey
    bne     -

    sei

    lda     #$01     ; $8000-$9FFF: ROM bank 1 (graphics data)
    tam     #$10
    
    ; Show the background image
    sti16 indPtr,bg_nametable
    stz     vramAddr
    stz     vramAddr+1
    ldy     #32
-:
    st0     #0
    lda     vramAddr
    sta.w   VDC_DATA_L
    lda     vramAddr+1
    sta.w   VDC_DATA_H
    st0     #2
    ldx     #44
--:
    lda     (<indPtr)
    ora     #$80
    sta.w   VDC_DATA_L
    INCW    indPtr
    stz.w   VDC_DATA_H
    INCW    indPtr
    dex
    bne     --
    lda     vramAddr
    clc
    adc     #64
    sta     vramAddr
    lda     vramAddr+1
    adc     #0
    sta     vramAddr+1
    dey
    bne     -

    ; Load the font tiles
    sti16   indPtr,font
    set_mawri $1000
    st0     #2
    jsr     load_font
    inc     <indPtr+1
    jsr     load_font
    inc     <indPtr+1
    jsr     load_font
    inc     <indPtr+1
    jsr     load_font
    inc     <indPtr+1
    jsr     load_font
    inc     <indPtr+1
    jsr     load_font
    inc     <indPtr+1
    jsr     load_font

    ; Load the background image tiles and palette
    set_mawri $0800
    st0     #2
    tia     bg_patterns,VDC_DATA_L,bg_patterns_end-bg_patterns

    stz.w   VCE_INDEX_L
    stz.w   VCE_INDEX_H
    tia     palette,VCE_DATA_L,palette_end-palette

    rts


plane2:
.db 255,255,0,0,255,255,0,0
plane3:
.db 255,255,255,255,0,0,0,0


load_font:
    cly
--:	
    ldx     #7
    sty     tempw
-:
    lda     (<indPtr),y
    iny
    sta.w   VDC_DATA_L
    lda     (<indPtr),y
    iny
    sta.w   VDC_DATA_H
    dex
    bpl     -
    ldx     #7
    ldy     tempw
-:
    lda     (<indPtr),y
    sta     temp
    lda.w   plane2,x
    and     temp
    sta.w   VDC_DATA_L
    lda.w   plane3,x
    and     temp
    sta.w   VDC_DATA_H
    iny
    iny
    dex
    bpl     -
    cpy     #0
    bne     --
    rts


read_joypad:
    lda     #3
    sta.w   GAMEPAD_IO
    lda     #1
    sta.w   GAMEPAD_IO
    lda.w   GAMEPAD_IO
    and     #$0F
    tax
    lda.w   key_bits_lo,x
    sta     joyData
    cla
    sta.w   GAMEPAD_IO
    lda.w   GAMEPAD_IO
    and     #$0F
    tax
    lda.w   key_bits_hi,x
    ora     joyData
    sta     joyData
    rts


print_current_nsf:
    ; Clear old text
    lda     #$43
    sta     vramAddr
    stz     vramAddr+1
    lda     #$11
    sta     textColor
    sti16   indPtr,spaces
    jsr     print_string
    lda     #$83
    sta     vramAddr
    stz     vramAddr+1
    sti16   indPtr,spaces
    jsr     print_string

    ; Game title
    lda     #$43
    sta     vramAddr
    stz     vramAddr+1
    lda     #$11
    sta     textColor
    lda     currentNSF
    jsr     calc_header_offset
    addi16  indPtr,$F80E
    jsr     print_string

    ; Copyright holder
    lda     #$83
    sta     vramAddr
    lda     currentNSF
    jsr     calc_header_offset
    addi16  indPtr,$F84E
    jsr     print_string

    ; Number of songs
    lda     currentNSF
    jsr     calc_header_offset
    addi16  indPtr,$F806
    lda     (<indPtr)
    sta     currMaxSong
    tax
    lda.w   binary_to_decimal,x
    sta     temp
    st0     #0
    st1     #$CD
    st2     #0
    st0     #2
    lsr     a
    lsr     a
    lsr     a
    lsr     a
    ora     #$10
    sta     VDC_DATA_L
    st2     #$11
    lda     temp
    and     #15
    ora     #$10
    sta     VDC_DATA_L
    st2     #$11
    rts

print_song_num:
    ldx     highlightedSong
    inx
    lda.w   binary_to_decimal,x
    sta     temp
    st0     #0
    st1     #$C8
    st2     #0
    st0     #2
    lsr     a
    lsr     a
    lsr     a
    lsr     a
    ora     #$10
    sta     VDC_DATA_L
    st2     #$11
    lda     temp
    and     #15
    ora     #$10
    sta     VDC_DATA_L
    st2     #$11
    rts


print_song_list:
    lda     #$C3
    sta     tempw
    lda     #$01
    sta     tempw+1
    stz     count1
-:
    lda     count1
    cmp     HEADER_LOCATION-4
    bcc     +
    ;beq		+
    bra     _psl_done
+:
    lda     tempw
    sta     vramAddr
    lda     tempw+1
    sta     vramAddr+1

    ldx     #$21

    lda     count1
    cmp     highlightedNSF
    bne     +
    ldx     #$11
+:
    stx     textColor
    jsr     calc_header_offset
    addi16  indPtr,$F80E
    jsr     print_string

    ; Move to the next entry in the table
    lda     tempw
    clc
    adc     #$40
    sta     tempw
    lda     tempw+1
    adc     #0
    sta     tempw+1

    inc     count1
    lda     count1
    cmp     #15
    bne     -
_psl_done:
    rts


print_string: 
    st0     #0
    lda     vramAddr
    sta.w   VDC_DATA_L
    lda     vramAddr+1
    sta.w   VDC_DATA_H
    st0     #2
    cly
-:
    cpy     #26
    beq     +
    lda     (<indPtr),y
    beq     +
    iny
    cmp     #97
    bcc     ++
    sec
    sbc     #32    ; toupper
++:
    sec
    sbc     #32
    sta.w   VDC_DATA_L
    lda     textColor
    sbc     #0
    sta.w   VDC_DATA_H
    inc     vramAddr
    jmp     -
+:
    rts



key_bits_lo:
    .db $0F,$07,$0E,$06,$0B,$03,$0A,$02
    .db $0D,$05,$0C,$04,$09,$01,$08,$00
key_bits_hi:
    .db $F0,$70,$B0,$30,$D0,$50,$90,$10
    .db $E0,$60,$A0,$20,$C0,$40,$80,$00



update_linear_counter:
    lda     chnEnable+2
    sta     temp

    lda     lcReloadFlag
    beq     +
    lda     <$4008
    and     #$7F
    sta     linearCounter    ; reload the linear counter if $400B was written to
    bra     ++
+:
    lda     linearCounter
    beq     +
    dec     linearCounter
++:
    bbs7    <$08,+
    stz     lcReloadFlag    ; clear the reload flag if $4008.7 is set
+:
    lda     linearCounter
    beq     +
    lda     chnLen+2
    beq     +
    ; Both the linear counter and length counter are non-zero; enable
    ; the channel if needed
    lda     chnEnable+2
    bne     ++
    lda     #$80
    and     chnEnableOverride+2
    sta     chnEnable+2
    lda     #$9F
    and     chnEnableOverride+2
    sta     chn3Ctrl
    bra     ++
+:
    ; Either the linear counter or the length counter is zero; disable the
    ; channel if needed
    lda     chnEnable+2
    beq     ++
    lda     #$00
    sta     chnEnable+2
    sta     chn3Ctrl
 ++:
    rts


update_length:
    lda     <$4015
    cmp     apuStatus2    ; has $4015 been written to?
    beq     +
    sta     apuStatus     ; yes, save the new value in apuStatus
+:
    ;-----------------------
    ; Square 1
    ;-----------------------
    lda     apuStatus
    bit     #1
    beq     +
    bbs5    <$00,+
    lda     chnLen
    stz     chnEnable
    beq     +
    dec     chnLen
    beq     ++
    lda     #$80
    sta     chnEnable
    bra     +
++:
    lda     chn0Ctrl
    and     #$7F
    sta     chn0Ctrl
+:

    ;-----------------------
    ; Square 2
    ;-----------------------
    lda     apuStatus
    bit     #2
    beq     +
    bbs5    <$04,+
    lda     chnLen+1
    stz     chnEnable+1
    beq     +
    dec     chnLen+1
    beq     ++
    lda     #$80
    sta     chnEnable+1
    bra     +
++:
    lda     chn2Ctrl
    and     #$7F
    sta     chn2Ctrl
+:

    ;-----------------------
    ; Triangle
    ;-----------------------
    lda     apuStatus
    bit     #4
    beq     ++
    bbs7    <$08,+
    lda     chnLen+2
    beq     +
    dec     chnLen+2
    bra     +
++:
    stz     chnLen+2
+:
    ; Updating chn3Ctrl is handled by the linear counter update
    ; function

    ;-----------------------
    ; Noise
    ;-----------------------
    lda     apuStatus
    bit     #8
    beq     +
    bbs5    <$0C,+
    lda     chnLen+3
    stz     chnEnable+3
    beq     +
    dec     chnLen+3
    beq     ++
    lda     #$80
    sta     chnEnable+3
    bra     +
++:
    lda     chn4Ctrl
    and     #$7F
    sta     chn4Ctrl
+:

    rts


set_chn0_envelope:
    lda     #0
    lda     chnEnv
    tax
    lda.w   logvolume,x
    ora     chnEnable
    and.w   chnEnableOverride

    beq     ++
    sta     tempw
    and     #15
    clc
    adc.w   chnOverdrive
    cmp     #16
    bcc     +++
    lda     #15
+++:
    sta     tempw+1
    lda     tempw
    and     #$F0
    ora     tempw+1
++:    
    sta     chn0Ctrl
    dec     chnEnv
    rts

set_chn2_envelope:
    lda     #2
    lda     chnEnv+1
    tax
    lda.w   logvolume,x
    ora     chnEnable+1
    and.w   chnEnableOverride+1

    beq     ++
    sta     tempw
    and     #15
    clc
    adc.w   chnOverdrive+1
    cmp     #16
    bcc     +++
    lda     #15
+++:
    sta     tempw+1
    lda     tempw
    and     #$F0
    ora     tempw+1
++:    
    sta     chn2Ctrl
    dec     chnEnv+1
    rts

set_chn4_envelope:
    lda     #4
    lda     chnEnv+3
    tax
    lda.w   logvolume,x
    ora     chnEnable+3
    and.w   chnEnableOverride+3

    beq     ++
    sta     tempw
    and     #15
    clc
    adc.w   chnOverdrive+3
    cmp     #16
    bcc     +++
    lda     #15
+++:
    sta     tempw+1
    lda     tempw
    and     #$F0
    ora     tempw+1
++:    
    sta     chn4Ctrl
    dec     chnEnv+3
    rts


update_envelope:
    ;-----------------------
    ; Square 1
    ;-----------------------
    bbs4    <$00,+     ; is the envelope enabled for square 1 ?
    dec     chnEnvCounter
    bpl     +
    lda     <$4000
    and     #$0F
    sta     chnEnvCounter
    lda     chnEnv
    bne     ++
    bbr5    <$00,+
    lda     #$0F
    sta     chnEnv    ; looping envelope; reset it
++:
    jsr     set_chn0_envelope
+:

    ;-----------------------
    ; Square 2
    ;-----------------------
    bbs4    <$04,+     ; is the envelope enabled for square 2 ?
    dec     chnEnvCounter+1
    bpl     +
    lda     <$4004
    and     #$0F
    sta     chnEnvCounter+1
    lda     chnEnv+1
    bne     ++
    bbr5    <$04,+
    lda     #$0F
    sta     chnEnv+1  ; looping envelope; reset it
++:
    jsr     set_chn2_envelope
+:

    ;-----------------------
    ; Noise
    ;-----------------------
    bbs4    <$0C,+     ; is the envelope enabled for noise ?
    dec     chnEnvCounter+3
    bpl     +
    lda     <$400C
    and     #$0F
    sta     chnEnvCounter+3
    lda     chnEnv+3
    bne     ++
    bbr5    <$0C,+
    lda     #$0F
    sta     chnEnv+3   ; looping envelope; reset it
++:
    jsr     set_chn4_envelope
+:
    rts



update_volume:
    ;-----------------------
    ; Square 1
    ;-----------------------
    bbr4    <$00,+     ; ignore the manual volume control if the envelope feature is enabled
    lda     apuRegs
    and     #$0F
    sta     temp
    lda     <$4000
    and     #$0F
    cmp     temp
    beq     +
    cmp     #0
    beq     ++
    tax
    lda.w   logvolume,x
++:
    ora     chnEnable
    sta     chn0Ctrl
+:

    ;-----------------------
    ; Square 2
    ;-----------------------
    bbr4    <$04,+     ; ignore the manual volume control if the envelope feature is enabled
    lda     apuRegs+4
    and     #$0F
    sta     temp
    lda     <$4004
    and     #$0F
    cmp     temp
    beq     +
    cmp     #0
    beq     ++
    tax
    lda.w   logvolume,x
++:
    ora     chnEnable+1
    sta     chn2Ctrl 
+:

    ;-----------------------
    ; Noise
    ;-----------------------
    bbr4    <$0C,+    ; ignore the manual volume control if the envelope feature is enabled
    lda     apuRegs+$C
    and     #$0F
    sta     temp
    lda     <$400C
    and     #$0F
    cmp     temp
    beq     +
    cmp     #0
    beq     ++
    tax
    lda.w   logvolume,x
++:
    ora     chnEnable+3
    sta     chn4Ctrl 
+:
    rts


update_duty:
    ;-----------------------
    ; Square 1
    ;-----------------------
    lda     apuRegs
    and     #$C0
    sta     temp
    lda     <$4000
    and     #$C0
    cmp     temp
    beq     +
    asl     a
    rol     a
    rol     a
    rol     a
    sta     samples    ; DEBUG
    tax
    lda.w  	sample_pointers,x
    sta     <indPtr
    lda.w   sample_pointers+1,x
    sta     <indPtr+1
    jsr     load_sq1_waveform
    lda     chn0Ctrl
    bbs4    <$00,++
    lda     chnEnv
    tax
    lda.w   logvolume,x
    ora     chnEnable
++
    sta     PSG_CHANNEL_CTRL
+:
    ;-----------------------
    ; Square 2
    ;-----------------------
    lda     apuRegs+4
    and     #$C0
    sta     temp
    lda     <$4004
    and     #$C0
    cmp     temp
    beq     +
    asl     a     ; x y000 0000
    rol     a     ; y 0000 000x
    rol     a     ; 0 0000 00xy
    rol     a     ; 0 0000 0xy0
    sta     samples+1  ; DEBUG
    tax
    lda.w  	sample_pointers,x
    sta     <indPtr
    lda.w   sample_pointers+1,x
    sta     <indPtr+1
    jsr     load_sq2_waveform
    lda     chn2Ctrl
    bbs4    <$04,++
    lda     chnEnv+1
    tax
    lda.w   logvolume,x
    ora     chnEnable+1
++
    sta     PSG_CHANNEL_CTRL
+:
    rts


update_sweep:
    lda     <$4001
    cmp     #$7D
    beq     +
    sta     sq1Swp
+:
    lda     <$4005
    cmp     #$7D
    beq     +
    sta     sq2Swp
+:
    ;-----------------------
    ; Square 1
    ;-----------------------
    lda     sq1Swp
    bpl     +++
    dec     chnSweepCounter
    bne     +++
    lsr     a
    lsr     a
    lsr     a
    lsr     a
    and     #7
    ina
    sta     chnSweepCounter
    lda     sq1Per
    sta     tempw
    lda     sq1Per+1
    sta     tempw+1
    lda     sq1Swp 
    and    	#7
    beq     +++
-:
    lsr     sq1Per+1
    ror     sq1Per
    dea
    bne     -
++:
    lda     sq1Swp
    and     #8
    bne     ++
    lda     sq1Per
    clc
    adc     tempw
    sta     <$4002
    lda     sq1Per+1
    adc     tempw+1
    sta     <$4003
    bpl     +++
    stz     <$4002
    stz     <$4003
    bra     +++
++:
    lda     sq1Per
    sec
    sbc     tempw
    sta     <$4002
    lda     sq1Per+1
    sbc     tempw+1
    sta     <$4003
    bmi     ++
    bne     +++
    lda     <$4002
    cmp     #8
    bcs     +++
++:
    stz     <$4002
    stz     <$4003
+++:

    ;-----------------------
    ; Square 2
    ;-----------------------
    lda     sq2Swp
    bpl     +++
    dec     chnSweepCounter+1
    bne     +++
    lsr     a
    lsr     a
    lsr     a
    lsr     a
    and     #7
    ina
    sta     chnSweepCounter+1
    lda     sq2Per
    sta     tempw
    lda     sq2Per+1
    sta     tempw+1
    lda     sq2Swp 
    and    	#7
    beq     +++
-:
    lsr     sq2Per+1
    ror     sq2Per
    dea
    bne     -
++:
    lda     sq2Swp
    and     #8
    bne     ++
    lda     sq2Per
    clc
    adc     tempw
    sta     <$4006
    lda     sq2Per+1
    adc     tempw+1
    sta     <$4007
    bpl     +++
    stz     <$4006
    stz     <$4007
    bra     +++
++:
    lda     sq2Per
    sec
    sbc     tempw
    sta     <$4006
    lda     sq2Per+1
    sbc     tempw+1
    sta     <$4007
    bmi     ++
    bne     +++
    lda     <$4006
    cmp     #8
    bcs     +++
++:
    stz     <$4006
    stz     <$4007
+++:

    ; Fall through to update_period


update_period:
    ;-----------------------
    ; Square 1
    ;-----------------------
    lda     <$4002
    cmp     apuRegs+2
    bne     +
    lda     <$4003
    cmp     invalidValue    ; has the period MSB been written to?
    beq     ++
    cmp     apuRegs+3       ; ..and is its value different from the previous one?
    beq     ++
+:
    lda     channelMap+0
    sta     PSG_CHANNEL_SELECT
    lda     <$4002
    sta     PSG_FREQ_FINE
    sta     sq1Per
    lda     <$4003
    cmp     invalidValue
    beq     ++
    and     #7
    sta     PSG_FREQ_COARSE
    sta     sq1Per+1
++:
    ; Mute the channel when the period is zero
    lda     #$FF
    sta     chnEnableOverride
    lda     <$4002
    bne     +
    lda     <$4003
    beq     ++
    cmp     invalidValue
    bne     +
++:
    lda     #$7F
    sta     chnEnableOverride
+:
    lda     chn0Ctrl
    ora     chnEnable
    and     chnEnableOverride
    sta     chn0Ctrl

    bbs4    <$00,+
    lda     <$4003
    cmp     invalidValue
    beq     +
    ; Restart envelope
    lda     <$4000
    and     #$0F
    sta     chnEnvCounter
    lda     #$0F
    sta     chnEnv
    jsr     set_chn0_envelope
+:
    lda     <$4003
    cmp     invalidValue
    beq     +
    lda     apuStatus
    bit     #1
    beq     +
    ; Reload length counter
    stz     chnEnable
    lda     <$4003
    lsr     a
    lsr     a
    lsr     a
    tax
    lda.w   length_counters,x
    lsr     a
    sta     chnLen
    beq     +
    lda     #$80
    sta     chnEnable
+:

    ;-----------------------
    ; Square 2
    ;-----------------------
    lda     <$4006
    cmp     apuRegs+6
    bne     +
    lda     <$4007
    cmp     invalidValue
    beq     ++
    cmp     apuRegs+7
    beq     ++
+:
    lda     channelMap+1
    sta     PSG_CHANNEL_SELECT
    lda     <$4006
    sta     PSG_FREQ_FINE
    sta     sq2Per
    lda     <$4007
    cmp     invalidValue
    beq     ++
    and     #7
    sta     PSG_FREQ_COARSE
    sta     sq2Per+1
++:
    lda     #$FF
    sta     chnEnableOverride+1
    lda     <$4006
    bne     +
    lda     <$4007
    beq     ++
    cmp     invalidValue
    bne     +
++:
    lda     #$7F
    sta     chnEnableOverride+1
+:
    lda     chn2Ctrl
    ora     chnEnable+1
    and     chnEnableOverride+1
    sta     chn2Ctrl

    bbs4    <$04,+
    lda     <$4007
    cmp     invalidValue
    beq     +
    ; Restart envelope
    lda     <$4004
    and     #$0F
    sta     chnEnvCounter+1
    lda     #$0F
    sta     chnEnv+1
    jsr     set_chn2_envelope
+:
    lda     <$4007
    cmp     invalidValue
    beq     +
    lda     apuStatus
    bit     #2
    beq     +
    ; Reload length counter
    stz     chnEnable+1
    lda     <$4007
    lsr     a
    lsr     a
    lsr     a
    tax
    lda.w   length_counters,x
    lsr     a
    sta     chnLen+1
    beq     +
    lda     #$80
    sta     chnEnable+1
+:

    ;-----------------------
    ; Triangle
    ;-----------------------
    lda     <$400B
    cmp     invalidValue
    beq     +
    sta     triPerHi
+:

    lda     <$400A
    cmp     apuRegs+$A
    bne     +
    lda     <$400B
    cmp     invalidValue
    beq     ++
    cmp     apuRegs+$B
    beq     ++
+:
    lda     channelMap+2 
    sta     PSG_CHANNEL_SELECT
    lda     <$400A
    asl     a     ; multiply period by 2 (divide frequency by 2)
    sta     PSG_FREQ_FINE
    lda     <$400A
    asl     a
    lda     triPerHi
    rol     a
    and     #$F
    sta     PSG_FREQ_COARSE
++:
    ; Mute the channel when the period is 0 or 1
    lda     #$FF
    sta     chnEnableOverride+2
    lda     <$400A
    cmp     #2
    bcs     +
    lda     triPerHi
    and     #7
    bne     +
    lda     #$7F
    sta     chnEnableOverride+2
+:
    lda     chn3Ctrl
    ora     chnEnable+2
    and     chnEnableOverride+2
    sta     chn3Ctrl

    lda     <$400B
    cmp     invalidValue
    beq     +
    lda     #1
    sta     lcReloadFlag
    lda     apuStatus
    bit     #4
    beq     +
    ; Reload length counter
    lda     <$400B
    lsr     a
    lsr     a
    lsr     a
    tax
    lda.w   length_counters,x
    lsr     a
    sta     chnLen+2
+:

    ;-----------------------
    ; Noise
    ;-----------------------
    lda     <$400E
    cmp     apuRegs+$E
    beq     ++
    lda     channelMap+3
    sta     PSG_CHANNEL_SELECT
    lda     <$400E
    bmi     +
    ; White noise
    and     #$0F
    tax
    lda.w   noise_periods,x
    sta     PSG_NOISE_CTRL
    bra     ++
+:
    ; Periodic noise
    stz     PSG_NOISE_CTRL
    and     #$0F
    asl     a
    tax
    lda.w   periodic_noise_periods,x
    sta     PSG_FREQ_FINE
    lda.w   periodic_noise_periods+1,x
    sta     PSG_FREQ_COARSE
++:
    bbs4    <$0C,+
    lda     <$400F
    cmp     invalidValue
    beq     +
    ; Restart envelope
    lda     <$400C
    and     #$0F
    sta     chnEnvCounter+3
    lda     #$0F
    sta     chnEnv+3
    jsr     set_chn4_envelope
+:
    lda     <$400F
    cmp     invalidValue
    beq     +
    lda     apuStatus
    bit     #8
    beq     +
    lda     chnEnable+3
    sta     temp
    ; Reload length counter
    stz     chnEnable+3
    lda     <$400F
    lsr     a
    lsr     a
    lsr     a
    tax
    lda.w   length_counters,x
    lsr     a
    sta     chnLen+3
    beq     +
    lda     #$80
    sta     chnEnable+3
    eor     temp
    beq     +
    lda     #$80
    ora     chn4Ctrl  ;Prev
    sta     chn4Ctrl
+:
    rts


; indPtr points to the 32-byte waveform
load_waveform:
    lda     #$40
    sta.w   PSG_CHANNEL_CTRL    ; reset waveform position
    stz.w   PSG_CHANNEL_CTRL    ; set waveform write mode
    cly
-:
    lda     (<indPtr),y
    sta.w   PSG_CHANNEL_WAVE
    iny
    cpy     #32
    bne     -
    rts

; Load a new waveform for Square1.
; Square1 will be moved to a free HuC6280 channel to avoid having to halt the
; currently used channel while loading the waveform.
; indPtr points to the 32-byte waveform
load_sq1_waveform:
    lda     freeChannel          ; load into the new channel's waveform RAM
    sta     PSG_CHANNEL_SELECT
    lda     #$40
    sta.w   PSG_CHANNEL_CTRL     ; reset waveform position
    stz.w   PSG_CHANNEL_CTRL     ; set waveform write mode
    cly
-:
    lda     (<indPtr),y
    sta.w   PSG_CHANNEL_WAVE
    iny
    cpy     #32
    bne     -
    lda     channelMap+0
    tay
    sta     PSG_CHANNEL_SELECT
    stz     PSG_CHANNEL_CTRL     ; disable the channel currently used for Square1
    lda     freeChannel
    sty     freeChannel          ; mark the old channel as free
    sta     channelMap+0
    sta     PSG_CHANNEL_SELECT
    lda     sq1Per
    sta     PSG_FREQ_FINE        ; set the frequency for the new channel
    lda     sq1Per+1
    sta     PSG_FREQ_COARSE
    lda     chn0Ctrl
    sta     PSG_CHANNEL_CTRL     ; ..and set the volume / enable flag
    rts
 
; Load a new waveform for Square2.
; Square2 will be moved to a free HuC6280 channel to avoid having to halt the
; currently used channel while loading the waveform. 
; indPtr points to the 32-byte waveform
load_sq2_waveform:
    lda     freeChannel
    sta     PSG_CHANNEL_SELECT
    lda     #$40
    sta.w   PSG_CHANNEL_CTRL     ; reset waveform position
    stz.w   PSG_CHANNEL_CTRL     ; set waveform write mode
    cly
-:
    lda     (<indPtr),y
    sta.w   PSG_CHANNEL_WAVE
    iny
    cpy     #32
    bne     -
    lda     channelMap+1
    tay
    sta     PSG_CHANNEL_SELECT
    stz     PSG_CHANNEL_CTRL
    lda     freeChannel
    sty     freeChannel
    sta     channelMap+1
    sta     PSG_CHANNEL_SELECT
    lda     sq2Per
    sta     PSG_FREQ_FINE
    lda     sq2Per+1
    sta     PSG_FREQ_COARSE
    lda     chn0Ctrl+1
    sta     PSG_CHANNEL_CTRL
    rts
    

start_timer:
    lda    delay
    sta    TIMER_COUNTER
    lda    #1
    sta    TIMER_CTRL
    lda    #$F9
    sta    INTERRUPT_CTRL    ; disable all interrupts except TIMER and VDC
    lda    #1
    sta    waitTimer
    rts



vdc_reg_table: 
    .db $06
    .dw $0000
    .db $07
    .dw $0000 
    .db $08
    .dw $0000
    .db $09
    .dw $0010    ; 64x32 BAT
    .db $0A
    .dw $0303    ; 352 pixels wide
    .db $0B
    .dw $062B    ; ....
    .db $0C
    .dw $0D02    ; 240 pixels tall
    .db $0D
    .dw $00EF    ; ....
    .db $0E
    .dw $0003
    .db $05
    .dw $0080
    .db $0F
    .dw $0000

binary_to_decimal: 
.db $00,$01,$02,$03,$04,$05,$06,$07,$08,$09
.db $10,$11,$12,$13,$14,$15,$16,$17,$18,$19
.db $20,$21,$22,$23,$24,$25,$26,$27,$28,$29
.db $30,$31,$32,$33,$34,$35,$36,$37,$38,$39
.db $40,$41,$42,$43,$44,$45,$46,$47,$48,$49
.db $50,$51,$52,$53,$54,$55,$56,$57,$58,$59
.db $60,$61,$62,$63,$64,$65,$66,$67,$68,$69
.db $70,$71,$72,$73,$74,$75,$76,$77,$78,$79
.db $80,$81,$82,$83,$84,$85,$86,$87,$88,$89
.db $90,$91,$92,$93,$94,$95,$96,$97,$98,$99

song_num:
.db "SONG 00 / 00",0

spaces:
.db "                          ",0


logvolume:
.db 0,$10,$14,$16,$18,$19,$1A,$1B
.db $1C,$1C,$1D,$1D,$1E,$1E,$1F,$1F


noise_periods:
; 447443 223721 118860 55930, 27965 18643 13982 11186, 8860 7046 4709 3523, 2348 1761 880 440 Hz
.db $9F,$9F,$9F,$9E, $9D,$9C,$9B,$9A, $99,$97,$93,$8F, $87,$80,$80,$80


periodic_noise_periods:
.dw 4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068


length_counters:
.db  10,254, 20,  2, 40,  4, 80,  6, 160,  8, 60, 10, 14, 12, 26, 14
.db  12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30

sample_pointers:
.dw square_wave_12_5, square_wave_25, square_wave_50, square_wave_75

; 12.5% square wave
square_wave_12_5:
.db 0,0,0,0,20,20,20,20, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0

; 25% square wave
square_wave_25:
.db 0,0,0,0,20,20,20,20, 20,20,20,20,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0

; 50% square wave
square_wave_50:
.db 0,0,0,0,20,20,20,20, 20,20,20,20,20,20,20,20, 20,20,20,20,0,0,0,0, 0,0,0,0,0,0,0,0

; 75% square wave
square_wave_75:
.db 20,20,20,20,0,0,0,0, 0,0,0,0,20,20,20,20, 20,20,20,20,20,20,20,20, 20,20,20,20,20,20,20,20

; Triangle wave
triangle_wave:
.db 30,28,26,24,22,20,18,16, 14,12,10,8,6,4,2,0, 0,2,4,6,8,10,12,14, 16,18,20,22,24,26,28,30

.ends



.bank 1 slot 4
.section "data"

font:
    .incbin "adore.chr"
bg_nametable:
    .incbin "hunter_bg.nam"
bg_patterns:
    .incbin "hunter_bg.chr"
bg_patterns_end:
palette:
    .incbin "hunter_bg.pal"
    .dw 0,0 ;,0,0 ;,0,0,0,0
    .dw 0,$1FF,$49,0,0,$1FF,0,0,0,$1FF,0,0,0,$1FF,0,0
    .dw 0,$115,$4B,0,0,$115,0,0,0,$115,0,0,0,$115,0,0
    .dw 0,$49,$49,$49, 0,$49,$49,$49, 0,$49,$49,$49, 0,$49,$49,$49
palette_end:


.ends
.org $1FFD
.db "mic"


.bank 2 slot 4
.section "data2"
.ends
.org $1FFD
.db "mic"


.bank 3 slot 0
.section "text0"

ram_code_start:

; This routine is moved to RAM. The reason is that it swaps in the last 8kB of the NSF at $E000 (which is
; the same bank that the HuC6280 playback code normally will be in) before jumping to the NSF code. So when
; the NSF code does its RTS it would return back to wrong place.
jsr_nsf:
    lda     currentNSF
    asl     a
    asl     a
    adc     #$07     ; $E000-$FFFF: ROM bank 7 (NSF cont.)
    tam     #$80
    lda     #$F8     ; $6000-$7FFF: RAM
    tam     #$08    
    lda     currentSong
    jsr     do_jump+RAM_CODE_BASE ;$1914
    lda     #$00     ; $E000-$FFFF: ROM bank 0 (playback code)
    tam     #$80
    lda     #$FF     ; $6000-$7FFF: I/O
    tam     #$08
    rts
do_jump:
    jmp     (jumpAddress)
jsr_nsf_end: 


; The timer interrupt handler also needs to be in RAM since the interrupt can occur while part of the NSF
; is swapped in at $E000.
timer_irq:
    pha
    tma     #$08
    pha
    lda     #$FF                ; $6000-$7FFF: I/O
    tam     #$08
    stz.w   INTERRUPT_STATUS    ; acknowledge interrupt
    stz.w   TIMER_CTRL
    pla
    tam     #$08
    stz     waitTimer
    pla
    rti
timer_irq_end:


vdc_irq:
    pha
    phx
    phy

    tma     #$08
    pha
    lda     #$FF         ; $6000-$7FFF: I/O
    tam     #$08

    lda     VDC_CTRL     ; acknowledge interrupt

    ; Update volume indicators
    lda     #9
    sta     VCE_INDEX_L
    stz     VCE_INDEX_H
    cly
-:
    lda.w   chnEnable,y
    and.w   chnEnableOverride,y
    beq     +
    lda.w   chn0Ctrl,y
    and     #15
+:
    asl     a
    tax
    lda.w   volumeter+RAM_CODE_BASE,x
    sta     VCE_DATA_L
    lda.w   volumeter+1+RAM_CODE_BASE,x
    sta     VCE_DATA_H
    iny
    cpy     #4
    bne     -

    ; Square waveform icons
    set_mawri $689
    st0     #2
    lda     <$4000
    asl     a
    rol     a
    rol     a
    and     #3
    clc
    adc     #96
    sta.w   VDC_DATA_L
    st2     #$31
    set_mawri $68C
    st0     #2
    lda     <$4004
    asl     a
    rol     a
    rol     a
    and     #3
    clc
    adc     #96
    sta.w   VDC_DATA_L
    st2     #$31

    ; Update the color of the "BOOST" and "SPEED" indicators
    ; if their states have changed
    lda     #6
    sta     VCE_INDEX_L
    stz     VCE_INDEX_H
    clx
    lda     chnOverdrive
    bne     +
    ldx     #$DB
+:
    stx     VCE_DATA_L
    stz     VCE_DATA_H
    clx
    lda     speedOverride
    beq     +
    ldx     #$DB
+:
    stx     VCE_DATA_L
    stz     VCE_DATA_H

    lda     updateSongInfo
    beq     +
    tma     #$80
    cmp     #0
    bne     +
    tii     $2010,f0backup,16
    jsr     print_song_list
    jsr     print_current_nsf
    jsr     print_song_num
    tii     f0backup,$2010,16
    stz     updateSongInfo
+:

    pla
    tam     #$08

    ply
    plx
    pla
irq2:
    rti

volumeter:
.dw 0,$40,$80,$48,$20,$28,$28,$30
.dw $38,$78,$B8,$F8,$138,$178,$1B8,$1F8

ram_code_end:

.ends
.org $1FFD
.db "mic"



