;Archivo:	       Main_lab_6.s
;dispositivo:	       PIC16F887
;Autor:		       Selvin E. Peralta
;Compilador:	       pic-as (v2.31), MPLABX V5.45
;
;Programa:	       Timer1 y Timer2
;Hardware:	       2 Display de 7 segmentos y led 
;
;Creado:	       23 mar, 2021
;Ultima modificacion:  27 mar, 2021
    
PROCESSOR 16F887
#include <xc.inc>

; configuración word1
 CONFIG FOSC=INTRC_NOCLKOUT //Oscilador interno sin salidas
 CONFIG WDTE=OFF	    //WDT disabled (reinicio repetitivo del pic)
 CONFIG PWRTE=ON	    //PWRT enabled (espera de 72ms al iniciar
 CONFIG MCLRE=OFF	    //pin MCLR se utiliza como I/O
 CONFIG CP=OFF		    //sin protección de código
 CONFIG CPD=OFF		    //sin protección de datos
 
 CONFIG BOREN=OFF	    //sin reinicio cuando el voltaje baja de 4v
 CONFIG IESO=OFF	    //Reinicio sin cambio de reloj de interno a externo
 CONFIG FCMEN=OFF	    //Cambio de reloj externo a interno en caso de falla
 CONFIG LVP=ON		    //Programación en bajo voltaje permitida
 
;configuración word2
  CONFIG WRT=OFF	//Protección de autoescritura 
  CONFIG BOR4V=BOR40V	//Reinicio abajo de 4V 
  
   PSECT udata_bank0 ;common memory
    cont:	 DS 2   ;1 byte apartado
    banderas:	 DS 1
    nibble:	 DS 2   ;2 byte apartado
    display_var: DS 2   ;2 byte apartado
    V1:          DS 2   ; VARIABLE CON EL VALOR DEL NUMERO A SEPARAR EN CEN, DEC Y UNI.
    Unid:        DS 1   ; VARIABLE CON EL NUMERO DE UNIDADES DEL VALOR DE V1.
    Dece:        DS 1   ; VARIABLE CON EL NUMERO DE DECENAS DEL VALOR DE V1.
    Cent:        DS 1   ; VARIABLE CON EL NUMERO DE CENTENAS DEL VALOR DE V1.
    UNID1:       DS 1   ; VARIABLE CON EL NUMERO DE LA UNIDAD A MOSTRAR EN DISPLAYS.  
    DECE1:       DS 1   ; VARIABLE CON EL NUMERO DE LA DECENA A MOSTRAR EN DISPLAYS.
    CENT1:       DS 1   ; VARIABLE CON EL NUMERO DE LA CENTENA A MOSTRAR EN DISPLAYS.
    
    
   PSECT udata_shr      ;common memory
    w_temp:	 DS  1  ;Variable para el w temporal 
    STATUS_TEMP: DS  1  ;Variable para el STATUS temporal
    
   PSECT resVect, class=CODE, abs, delta=2
  ;----------------------Macros------------------------------
   reiniciar_Tmr0 macro ;Activamos el macros de reinicio de Tmr0
    banksel TMR0        ;Activamos el puerto del timer0
    movlw   237	; 5 ms  ;Tiempo que se guardara para el timer0 en w
    movwf   TMR0        ;Luego guardamos el valor de w a TMR0
    bcf	    T0IF        ;Seteamos la bandera T0IF
    endm
  ;----------------------vector reset------------------------
  ORG 00h	        ;posición 000h para el reset
  resetVec:
    PAGESEL main
    goto main
    
  PSECT code, delta=2, abs
ORG 100h	        ;posicion para el codigo
;------------------ TABLA -----------------------
Tabla:
    clrf    PCLATH
    bsf	    PCLATH, 0
    andlw   0x0F
    addwf   PCL
    retlw   00111111B;0
    retlw   00000110B;1
    retlw   01011011B;2
    retlw   01001111B;3
    retlw   01100110B;4
    retlw   01101101B;5
    retlw   01111101B;6
    retlw   00000111B;7
    retlw   01111111B;8
    retlw   01101111B;9
    retlw   01110111B;A
    retlw   01111100B;b
    retlw   00111001B;c
    retlw   01011110B;d
    retlw   01111001B;E
    retlw   01110001B;F

PSECT intVect, class=CODE, abs, delta=2

  ;----------------------interrupción reset------------------------
  ORG 04h	            ;posición 0004h para interr
  push:			    
    movf    w_temp	    ;Guardamos w en una variable temporal
    swapf   STATUS, W	    ;Sustraemos el valor de status a w sin tocar las interrupciones
    movwf   STATUS_TEMP	    ;Guardamos el status que acabamos de guardar en una variable temporal
    
  isr:
    btfsc   T0IF	    ;Si el timer0  levanta ninguna bandera de interrupcion
    call    TMR0_interrupt  ;Rutina de interrupcion del timer0
    
    btfsc   TMR1IF	    ;Si el timer1  levanta ninguna bandera de interrupcion
    call    TMR1_interrupt  ;Rutina de interrupcion del timer0
    
    btfsc   TMR2IF	    ;Si el timer1  levanta ninguna bandera de interrupcion
    call    TMR2_interrupt  ;Rutina de interrupcion del timer0
  pop:
    swapf   STATUS_TEMP, W  ;Recuperamos el valor del status original
    movwf   STATUS	    ;Regresamos el valor a Status
    swapf   w_temp, F	    ;Guardamos el valor sin tocar las banderas a F
    swapf   w_temp, W	    ;El valor normal lo dejamos en w
    retfie		    ;Salimos de las interrupciones
    
;---------SubrutinasInterrupción-----------    
TMR0_interrupt:
    reiniciar_Tmr0          ;Utilizamos el macro de reinicio de Tmr0
    bcf	    STATUS, 0	    ;Dejo el STATUS 0 en un valor de 0
    clrf    PORTD           ;Limpio el puerto D
    btfss   PORTB,0	    
    goto    transistor      ;Vamos a la subrrutina que sera afectado por el timer2
    btfsc   banderas, 1	    ;La variable bandera ayuda para realizar los saltos en los display 
    goto    display0	    ;Si la variable vandera es 1 en la posicion 1 vamos a la rutina del display0
    btfsc   banderas, 2 
    goto    display1        
    movlw   00000001B       ;Se colocara un 1 dentro de la variable para poder correr ese numero y asi crear el salto de display 
    andlw   0x0F	    ;Se multiplica por 0F porque solo nos interezan los primero 4 bits ya que solo son dos display
    movwf   banderas	    ;Se introduce el valor a la varialbe banderas 
siguientedisplay:
    RLF	    banderas, 1	    ;Me ayuda a cambiar la bandera e ir al siguiente display
    return
    
transistor:
    bcf PORTB,3
    bcf PORTB,4
    return
    
display0:		    
    movf    UNID1, w        ;La variable display tiene el valor que necesito ya modificado para hexadecimal
    movwf   PORTC	    ;Despues de pasar display_var a w movemos w al puertoC
    bsf	    PORTD, 0	    ;seteamos el pin del puerto D para controlar que display se mostrara
    goto    siguientedisplay
    
display1:
    movf    DECE1, w
    movwf   PORTC
    bsf	    PORTD, 1
    goto    siguientedisplay
    
    
TMR1_interrupt:
    movlw   0x85
    movwf   TMR1H
	
    movlw   0xEE
    movwf   TMR1L
    bcf	    TMR1IF
    incf    PORTA
    incf    cont
    return

TMR2_interrupt:
    clrf    TMR2
    bcf	    TMR2IF
    incf    PORTB
    return
;-----------configuracion----------------------------  
main:
    banksel ANSEL	   ;configurar como digital
    clrf    ANSEL
    clrf    ANSELH
    
    banksel TRISA	   ;configurar como salida los puertos seleccionados
    clrf    TRISA
    bcf	    TRISB, 0
  
   
    clrf    TRISC
    
    bcf	    TRISD, 0
    bcf	    TRISD, 1
    
    banksel PORTA	    ;reiniciar los puertos
    clrf    PORTA
    clrf    PORTC
    clrf    PORTD
    clrf    PORTB
    
    call    config_reloj    ;Configuracion de reloj para darle un valor al oscilador
    call    config_tmr0     ;Configuraciones del TMR0
    call    config_tmr1     ;Configuraciones del TMR1
    call    config_tmr2     ;Configuraciones del TMR2
    call    config_IE	    ;Configuracion de las interrupciones del timer0
 
loop:
    movf    cont, w        ;Pasamos el valor de la variable a W
    movwf   cont+1         ;Luego lo movemos a la nueva varaibles 
    movwf   V1             ;Luego se mueve a V1
    call    Centenas       ; SEPARAMOS EL VALOR DE "V1" EN CENTENAS, DECENAS Y UNIDADES.
    call    DISPLAY_UDC    ; LE ASIGNAMOS A LAS CENT,DEC,UNID SU VALOR CORRESPONDIENTE EN EL DISPLAY DECIMAL.
    goto    loop
    
separar_nibbles:
    movf    cont+1, w	    ;Var tiene el valor del contador
    andlw   0x0f	    ;Obtenemos los 4 bits menos significativos
    movwf   nibble	    ;Los pasamos a nibble
    swapf   cont+1, w	    ;Volteamos la variable var
    andlw   0x0f	    ;Obtenemos los 4 bits mas significativos
    movwf   nibble+1	    ; Los pasamos a nibble+1
    return

config_displays:
    movf    nibble, w	    ;Movemos el valor de nibble a w
    call    Tabla	    ;Movemos w a la tabla
    movwf   display_var	    ;el valor de w preparado para el display lo madamos a la variable display_var
    movf    nibble+1, w	    ;
    call    Tabla
    movwf   display_var+1
    return
    
config_reloj:
    banksel OSCCON	    ;Banco OSCCON 
    bsf	    IRCF2	    ;OSCCON configuración bit2 IRCF
    bcf	    IRCF1	    ;OSCCON configuracuón bit1 IRCF
    bcf	    IRCF0	    ;OSCCON configuración bit0 IRCF
    bsf	    SCS		    ;reloj interno , 250kHz
    return  

config_tmr0:
    banksel OPTION_REG	    ;Banco de registros asociadas al puerto A
    bcf	    T0CS	    ; reloj interno clock selection
    bcf	    PSA		    ;Prescaler 
    bsf	    PS2
    bsf	    PS1
    bsf	    PS0		    ;PS = 111 Tiempo en ejecutar , 256
    
    banksel TMR0
    movlw   254	
    movwf   TMR0
    bcf	    T0IF	    ;2 ms
    return

config_tmr1:
    banksel T1CON 
    bcf	    TMR1GE
    bsf	    T1CKPS1         ;Preescaler 1:8
    bsf	    T1CKPS0
    bcf	    T1OSCEN         ;Reloj interno
    bcf	    TMR1CS
    bsf	    TMR1ON          ;Encendemos el TMR1
    movlw   0x85
    movwf   TMR1H
    movlw   0xEE
    movwf   TMR1L
    bcf	    TMR1IF
    return

config_tmr2:
    banksel T2CON
    movlw   11111111B
    movwf   T2CON
    
    Banksel PR2
    movlw   244
    movwf   PR2
    return
    
config_IE:
    banksel PIE1
    bsf	    TMR1IE	    ;Activamos la interrupciones del Tmer1
    bsf	    TMR2IE          ;Activamos la interrupciones del Tmer2
    bsf	    T0IE	    ;Activamos la interrupciones del Tmer0
    banksel T1CON
    bsf	    GIE	            ;Habilitar en general las interrupciones, Globales
    bsf	    PEIE		
    bcf	    TMR1IF	    ;Habilitamos el vamor maximo del timer1
    bcf	    TMR2IF          ;Habilitamos el vamor maximo del timer1
    bcf	    T0IF	    ;Limpiamos bandera
    return

    
 Centenas:
   CLRF   Cent              ;LIMPIAMOS LA VARIABLE CENT.
   MOVLW  01100100B         ;ASIGNAMOS EL VALOR DE "100" A "W".
   SUBWF  V1,1              ;RESTMOS "100" AL VALOR DE V1.
   BTFSS  STATUS,0          ;VERIFICAMOS SI EL RESULTADO DE LA RESTA ES NEGATIVO. 
   GOTO   Decenas           ;SI LA RESTA ES NEGATIVA, PASAMOS A LA SUBRUTINA DECENAS.
   INCF   Cent              ;SI LA RESTA AUN ES POSITVA O CERO, INCREMENTA "CENT".
   GOTO   $-5               ;REGRESAMOS 5 LINEAS PARA RESTAR NUEVAMENTE.
          
 Decenas: 
    clrf  Dece              ; LIMPIAMOS LA VARIABLE "DECE".
    MOVLW 01100100B         ; ASIGNAMOS EL VALOR DE "100" A "W".
    ADDWF V1,1              ; LE SUMAMOS 100 A V1, PARA QUE NOS QUEDE EL RESIDUO DE LA RESTA ANTERIOR.
    MOVLW 00001010B         ; ASIGNAMOS EL VALOR DE "10" A "W".
    SUBWF V1,1              ; RESTMOS "10" AL VALOR DE V1. 
    BTFSS STATUS,0          ; VERIFICAMOS SI EL RESULTADO DE LA RESTA ES NEGATIVO.
    GOTO  Unidades          ; SI LA RESTA ES NEGATIVA, PASAMOS A LA SUBRUTINA UNIDADES.
    incf  Dece              ; SI LA RESTA AUN ES POSITVA O CERO, INCREMENTA "DECE".
    GOTO  $-5               ; REGRESAMOS 5 LINEAS PARA RESTAR NUEVAMENTE.
    
 Unidades:
    clrf  Unid              ; LIMPIAMOS LA VARIABLE "UNID".
    movlw 00001010B         ; ASIGNAMOS EL VALOR DE "10" A "W". 
    addwf V1,1              ; LE SUMAMOS 10 A V1, PARA QUE NOS QUEDE EL RESIDUO DE LA RESTA ANTERIOR.
    movlw 00000001B         ; ASIGNAMOS EL VALOR DE "1" A "W".
    subwf V1,1              ; RESTMOS "1" AL VALOR DE V1. 
    btfss STATUS,0          ; VERIFICAMOS SI EL RESULTADO DE LA RESTA ES NEGATIVO.
    return                  ; SI LA RESTA ES NEGATIVA, REGRESAMOS AL LOOP.
    incf  Unid              ; SI LA RESTA AUN ES POSITVA O CERO, INCREMENTA "UNID".
    goto  $-5               ; REGRESAMOS 5 LINEAS PARA RESTAR NUEVAMENTE.

DISPLAY_UDC:          
    movf  Cent,W            ; MOVEMOS EL VALOR DE "CENT" A "W".
    call  Tabla             ; ASIGNAMOS A "W" EL VALOR CORRESPONDIENTE PARA EL DISPLAY. 
    movwf CENT1             ; MOVEMOS EL VALOR DE "W" A "CENT1".
    movf  Dece,W            ; MOVEMOS EL VALOR DE "DECE" A "W". 
    call  Tabla             ; ASIGNAMOS A "W" EL VALOR CORRESPONDIENTE PARA EL DISPLAY. 
    movwf DECE1             ; MOVEMOS EL VALOR DE "W" A "DECE1".
    movf  Unid,W            ; MOVEMOS EL VALOR DE "UNID" A "W". 
    call  Tabla             ; ASIGNAMOS A "W" EL VALOR CORRESPONDIENTE PARA EL DISPLAY. 
    movwf UNID1             ; MOVEMOS EL VALOR DE "W" A "UNID1".
    return   
    
    
    
END