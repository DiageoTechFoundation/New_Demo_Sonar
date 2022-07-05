*----------------------------------------------------------------------*
* Program   :ZMMFGI032                                                 *
* Created by:Noreen Prila                                              *
* Created on:March 13,2006                                             *
* Change/Transport Number:DA2K920819                                   *
* Report Description     :The interface objective is to supply Diageo  *
*        vendors with the information they require about the materials *
*        provided by Diageo that they consume.The new interface will   *
*        send information that is maintained in the SAP Material Master*
*        via a standardized IDOC format.The information will be sent   *
*        through an intermediate Platform that will do the mapping and *
*        translation to send the data to the subcontractor system.     *
*        Material Master                                               *
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Date   Modified by   Description                    Change Request # *
* ====   ===========   ===========                    ================ *
* 03/13/06 PRILANO     Initial Development            DA2K920819       *
* 10/19/07 MATEOJA     R11 UNITY DEFECT 3598          CD0K912823       *
* 06/12/07 SANCHEM     R11 UNITY DEFECT 542
*----------------------------------------------------------------------*
REPORT zmmfgi032 MESSAGE-ID zmi32
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 110
                 LINE-COUNT 65(2).

*----------------------------------------------------------------------*
*        TABLE DECLARATION                                             *
*----------------------------------------------------------------------*
*  In this section you can declare all tables that you are going to    *
*  use in your program.                                                *
*----------------------------------------------------------------------*
TABLES: t001w,       "Plants/Branches
        t134,        "Material Types
        t141,        "Material Status from Materials Management/PPC View
        mara,        "General Material Data
        makt,        "Material Descriptions
        marc,        "Plant Data for Material
        marm,        "Units of Measure for Material
        cdhdr,       "Change document header
        mean,        "International Article Numbers (EANs) for Material
        t9con.       "EDI - Conversion table


*----------------------------------------------------------------------*
*       TYPES OR TYPE-POOLS					       *	
*----------------------------------------------------------------------*
* In this section you can declare all type definitions that you are    *
* going to use in your program.					       *
*----------------------------------------------------------------------*
TYPES: BEGIN OF t_cdhdr,
        objectclas LIKE cdhdr-objectclas,
        objectid   LIKE cdhdr-objectid,
        changenr   LIKE cdhdr-changenr,
        tcode      LIKE cdhdr-tcode,
        udate      LIKE cdhdr-udate,
        change_ind LIKE cdhdr-change_ind,
       END OF t_cdhdr,
       BEGIN OF t_cdpos,
        changenr   LIKE cdpos-changenr,
	 tabname    LIKE cdpos-tabname,
	 fname      LIKE cdpos-fname,
        chngind    LIKE cdpos-chngind,
	 objectid   LIKE cdpos-objectid,
       END OF t_cdpos,

       BEGIN OF t_cdpos2,
        changenr   LIKE cdpos-changenr,
	 tabname    LIKE cdpos-tabname,
	 fname      LIKE cdpos-fname,
      	 objectid   LIKE mara-matnr,
       END OF t_cdpos2,

       BEGIN OF t_mara,
        matnr      LIKE mara-matnr,
        mtart      LIKE mara-mtart,
        ean11      LIKE mara-ean11,
        meins      LIKE mara-meins,
        bstme      LIKE mara-bstme,
        brgew      LIKE mara-brgew,
        ntgew      LIKE mara-ntgew,
        gewei      LIKE mara-gewei,
        volum      LIKE mara-volum,
        voleh      LIKE mara-voleh,
       END OF t_mara,

       BEGIN OF t_marc,
        matnr      LIKE marc-matnr,
        werks      LIKE marc-werks,
        mmsta      LIKE marc-mmsta,
        lvorm      LIKE marc-lvorm,
        mmstd      LIKE marc-mmstd,
        ausme      LIKE marc-ausme,
        beskz      LIKE marc-beskz,
        sobsl      LIKE marc-sobsl,
        rgekz      LIKE marc-rgekz,
        kausf      LIKE marc-kausf,
        nfmat      LIKE marc-nfmat,
       END OF t_marc,
       BEGIN OF t_makt,
        matnr      LIKE makt-matnr,
        maktx      LIKE makt-maktx,
       END OF t_makt,
       BEGIN OF t_marm,
        matnr      LIKE marm-matnr,
        meinh      LIKE marm-meinh,
        umrez      LIKE marm-umrez,
        umren      LIKE marm-umren,
       END OF t_marm,
       BEGIN OF t_output,
        msgfn     LIKE e1stzum-msgfn,
        mtart     LIKE mara-mtart,
        matnr     LIKE mara-matnr,
        maktx     LIKE makt-maktx,
        ean11     LIKE mara-ean11,
        lvorm     LIKE marc-lvorm,
        meins     LIKE mara-meins,
        bstme     LIKE mara-bstme,
        brgew     LIKE mara-brgew,
        ntgew     LIKE mara-ntgew,
        gewei     LIKE mara-gewei,
        volum     LIKE mara-volum,
        voleh     LIKE mara-voleh,
        werks     LIKE marc-werks,
        mmsta     LIKE marc-mmsta,
        mmstd     LIKE marc-mmstd,
        ausme     LIKE marc-ausme,
        beskz     LIKE marc-beskz,
        sobsl     LIKE marc-sobsl,
        rgekz     LIKE marc-rgekz,
        kausf     LIKE marc-kausf,
        nfmat     LIKE marc-nfmat,
        counter   TYPE n,
        meinh     LIKE marm-meinh,
        umrez     LIKE marm-umrez,
        umren     LIKE marm-umren,
       END OF t_output,

BEGIN OF t_counter,
        matnr     LIKE mara-matnr,
        counter   TYPE i,
       END OF t_counter.

TYPES: BEGIN OF t_status,
        matnr  LIKE mara-matnr,
        status LIKE edidc-status,
        docnum LIKE edidc-docnum,
        txt LIKE teds2-descrp,
       END OF t_status.

TYPES: BEGIN OF t_statxt,
        value LIKE teds1-status,
        descrp LIKE teds2-descrp,
        direct LIKE teds1-direct,
        layer LIKE teds1-layer,
        lay_descrp LIKE dd07v-ddtext,
       END OF t_statxt.

TYPES: BEGIN OF t_idoc_data.
        INCLUDE STRUCTURE edidd.
TYPES: END OF t_idoc_data.

TYPES: BEGIN OF t_edidc.
        INCLUDE STRUCTURE edidc.
TYPES: END OF t_edidc.

*ADD internal tables declaration
TYPES: BEGIN OF t_ausp,        "Maturing indicator
	  objek LIKE ausp-objek,
	  atinn LIKE ausp-atinn,
	  atwrt LIKE ausp-atwrt,
	END OF t_ausp.

DATA: i_ausp TYPE STANDARD TABLE OF t_ausp,
      x_ausp TYPE t_ausp.

TYPES: BEGIN OF t_ausp2,      "Maturing Plant
	  objek LIKE ausp-objek,
	  atinn LIKE ausp-atinn,
	  atwrt LIKE ausp-atwrt,
	END OF t_ausp2.

DATA: i_ausp2 TYPE STANDARD TABLE OF t_ausp2,
      x_ausp2 TYPE t_ausp2.


*ADD INTERNAL TABLE DECLARATION
TYPES: BEGIN OF t_t9con.
        INCLUDE STRUCTURE t9con.
TYPES: END OF t_t9con.

DATA: i_t9con1 TYPE STANDARD TABLE OF t_t9con,
      x_t9con1 TYPE t_t9con.

DATA: i_t9con2 TYPE STANDARD TABLE OF t_t9con,
      x_t9con2 TYPE t_t9con.

*----------------------------------------------------------------------*
*        DATA/VARIABLE DECLARATION                                     *
*----------------------------------------------------------------------*
*  In this section you can define internal tables,variables and etc.   *
*----------------------------------------------------------------------
DATA: v_temp     TYPE c,                 "temporary storage
      v_lines    TYPE i,                 "counter storage
      v_plnorder TYPE i,
      v_seglines TYPE i,
      v_success  TYPE i,
      v_error    TYPE i,
      v_txt      LIKE teds2-descrp.

DATA: i_cdhdr    TYPE STANDARD TABLE OF t_cdhdr,
      i_cdpos    TYPE STANDARD TABLE OF t_cdpos,
      i_cdpos2   TYPE STANDARD TABLE OF t_cdpos2,
      i_mara     TYPE STANDARD TABLE OF t_mara    WITH HEADER LINE,
      i_marc     TYPE STANDARD TABLE OF t_marc    WITH HEADER LINE,
      i_makt     TYPE STANDARD TABLE OF t_makt    WITH HEADER LINE,
      i_marm     TYPE STANDARD TABLE OF t_marm    WITH HEADER LINE,
      i_output   TYPE STANDARD TABLE OF t_output  WITH HEADER LINE,
      i_status TYPE STANDARD TABLE OF t_status    WITH HEADER LINE,
      i_status_2 TYPE STANDARD TABLE OF t_status  WITH HEADER LINE,
      i_counter  TYPE STANDARD TABLE OF t_counter,
      x_cdhdr    TYPE t_cdhdr,
      x_cdpos    TYPE t_cdpos,
      x_cdpos2   TYPE t_cdpos2,
      x_mara     TYPE t_mara,
      x_marc     TYPE t_marc,
      x_makt     TYPE t_makt,
      x_marm     TYPE t_marm,
      x_output   TYPE t_output,
      x_output2   TYPE t_output,
      x_output1  TYPE e1maram,
      x_status   TYPE t_status,
      x_status2  TYPE t_status,
      x_counter  TYPE t_counter,
      i_edidd    TYPE edidd OCCURS 0,
      x_edidd    TYPE edidd.

DATA: i_idoc_data TYPE STANDARD TABLE OF t_idoc_data,
      x_idoc_data TYPE t_idoc_data.

DATA: i_edidc TYPE STANDARD TABLE OF t_edidc ,
      x_edidc  TYPE t_edidc.

DATA: i_statxt TYPE STANDARD TABLE OF t_statxt,
      x_statxt  TYPE t_statxt.

DATA: x_control_rec TYPE t_edidc.

DATA: x_e1maram TYPE e1maram,
      x_e1maktm TYPE e1maktm,
      x_e1marcm TYPE e1marcm,
      x_e1marmm TYPE e1marmm.


*{begin of change guamosch MUSIC 06/22/2006 DA2K925080
*ADD data declaration
DATA: v_latinn LIKE ausp-atinn,
      x_e1mtxhm TYPE e1mtxhm.
*}end of change guamosch MUSIC 06/22/2006 DA2K925080

***}START OF INSERT MATEOJA 10/19/07 R11 UNITY DEFECT 3598 CD0K912823
DATA: v_rcvprn LIKE zplant_partners-rcvprn,
      v_zmeins LIKE zplant_partners-zmeins,
      v_old_value LIKE plfh-mgvgw,
      v_new_value LIKE plfh-mgvgw,
      v_old_unit LIKE t006-msehi,
      v_new_unit LIKE t006-msehi,
      v_conversion TYPE i.

CONSTANTS: c_01(2) VALUE '01',
           c_02(2) VALUE '02'.
***{END OF INSERT MATEOJA 10/19/07 R11 UNITY DEFECT 3598 CD0K912823


RANGES: r_fname   FOR cdpos-fname,
        r_tabname FOR cdpos-tabname.

*----------------------------------------------------------------------*
*         CONSTANTS DECLARATION                                        *
*----------------------------------------------------------------------*
*  Constants are named data objects that you create statically using   *
*  a declarative statement. They allow you to store data under a       *
*  particular name within the memory area of a program.                *
*  The value of a constant cannot be changed during the execution of   *
*  the program.                                                        *
*----------------------------------------------------------------------*

CONSTANTS:
           c_i(1)         TYPE c VALUE 'I',           "I
           c_eq(2)        TYPE c VALUE 'EQ',          "EQ
           c_mestyp(6)    TYPE c VALUE 'MATMAS',      "message type
           c_doctyp(8)    TYPE c VALUE 'MATMAS05',    "idoc type
           c_segnam(7)    TYPE c VALUE 'E1MARAM',     "segment name
           c_e(1)                VALUE 'E',           "error
           c_s(1)                VALUE 'S',           "success
           c_date(4)      TYPE c VALUE 'Date',       "date
           c_time(4)      TYPE c VALUE 'Time',       "time
           c_page(4)      TYPE c VALUE 'Page',       "page
           c_plant(5)            VALUE 'Plant',
           c_to(2)               VALUE 'TO',
           c_x(1)                VALUE 'X',
           c_logxt(4)            VALUE 'LOGS',
           c_error(5)            VALUE 'ERROR',
           c_53(2)               VALUE '53',
           c_03(2)               VALUE '03',
           c_30(2)               VALUE '30',
           c_0(1)                VALUE '0',
           c_1(1)                VALUE '1',
           c_mf(2)               VALUE 'MF',
           c_e1marmm(7)          VALUE 'E1MARMM',
           c_e1maktm(7)          VALUE 'E1MAKTM',
           c_e1marcm(7)          VALUE 'E1MARCM',
           c_e1maram(7)          VALUE 'E1MARAM'.


*ADD data declaration
CONSTANTS:  c_mat_plant(14)  TYPE c VALUE 'MATURING_PLANT',
            c_mat_indict(18) TYPE c VALUE 'MATURING_INDICATOR',
            c_e1mtxhm(7)     TYPE c VALUE 'E1MTXHM'.

*{BEGIN OF CHANGE GUAMOSCH MUSIC 07/27/2006 DA2K926298
*ADD CONSTANTS DECLARATION
CONSTANTS: c_3p(2)     TYPE c VALUE '3P',
           c_table(5)  TYPE c VALUE 'TABLE',
           c_field(5)  TYPE c VALUE 'FIELD'.
*}END OF CHANGE GUAMOSCH MUSIC 07/27/2006 DA2K926298


*----------------------------------------------------------------------*
*        SELECTION-SCREEN BEGIN / END				       *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
*----------------------------------------------------------------------*
*        SELECT-OPTIONS                                                *
*----------------------------------------------------------------------*
*  You can use this section to declare selection tables and create     *
*  corresponding input fields on the associated selection screen.      *
*----------------------------------------------------------------------*
SELECT-OPTIONS: s_werks FOR t001w-werks OBLIGATORY,   "plant
                s_matnr  FOR mara-matnr,              "material number
                s_mtart FOR t134-mtart  OBLIGATORY,   "material type
                s_mmsta FOR t141-mmsta,               "mat. status
                s_date  FOR sy-datum OBLIGATORY.      "date cretion
.
*----------------------------------------------------------------------*
*        PARAMETERS                                                    *
*----------------------------------------------------------------------*
*  You use the PARAMETERS statement to declare variables, similarly to *
*  the DATA statement Variables declared with the PARAMETERS statement *
*  are called parameters. For each parameter declared, an input field  *
*  appears on the corresponding selection screen.                      *
*  Please remember to add selection texts.                             *
*----------------------------------------------------------------------*
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*        INITIALIZATION                                                *
*----------------------------------------------------------------------*
*  This event occurs before the standard selection screen is called.   *
*  You can use it, for example, to initialize the input fields of the  *
*  standard selection screen.                                          *
*  This is the only possible way to change the default values of       *
*  parameters or selection criteria defined in logical databases.      *
*----------------------------------------------------------------------*
INITIALIZATION.
  CLEAR:   v_temp,
           v_success,
           v_seglines,
           v_plnorder,
           v_error.
  REFRESH: i_status_2,
           i_status,
           i_output,
           i_marm,
           i_marc,
           i_mara,
           i_makt,
           i_cdpos2,
           i_cdpos,
           i_counter,
           i_cdhdr.



  REFRESH: i_t9con2.
  SELECT *
        FROM t9con
        INTO TABLE i_t9con2
        WHERE key1 = c_3p AND
              key2 = c_field.

  IF sy-subrc EQ 0.
  ENDIF.

  CLEAR: x_t9con2.
  LOOP AT i_t9con2 INTO x_t9con2.

    PERFORM add_range USING x_t9con2-key3.

    CLEAR: x_t9con2.
  ENDLOOP.

  REFRESH: i_t9con1.
  SELECT *
        FROM t9con
        INTO TABLE i_t9con1
        WHERE key1 = c_3p AND
              key2 = c_table.

  IF sy-subrc EQ 0.
  ENDIF.

  CLEAR: x_t9con1.
  LOOP AT i_t9con1 INTO x_t9con1.

    PERFORM add_table USING x_t9con1-key3.

    CLEAR: x_t9con1.
  ENDLOOP.




*----------------------------------------------------------------------*
*        AT SELECTION-SCREEN                                           *
*----------------------------------------------------------------------*
*  This event is the basic form of a whole series of events that       *
*  occur while the selection screen is being processed.                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*--Check plant
  CLEAR: v_temp.
  SELECT SINGLE werks
         FROM t001w
         INTO v_temp
         WHERE werks IN s_werks.

  IF sy-subrc NE 0.
    MESSAGE text-005 TYPE c_e.
  ENDIF.

*--Check material type
  CLEAR: v_temp.
  SELECT SINGLE mtart
         FROM t134
         INTO v_temp
         WHERE mtart IN s_mtart.

  IF sy-subrc NE 0.
    MESSAGE text-006 TYPE c_e.
  ENDIF.

*--Check Plant specific material status
  CLEAR: v_temp.
  SELECT SINGLE mmsta
         FROM t141
         INTO v_temp
         WHERE mmsta IN s_mmsta.

  IF sy-subrc NE 0.
    MESSAGE text-007 TYPE c_e.
  ENDIF.


AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
*        START-OF-SELECTION                                            *
*----------------------------------------------------------------------*
*  This event occurs after the selection screen has been processed and *
*  before data is read using the logical database.                     *
*----------------------------------------------------------------------*
START-OF-SELECTION.
*If the all Bills of Material radio botton is marked then delete the
*value of the field creation/Modification and modify the *attribute
*field to NO-INPUT also.
* check if the date field has values when RB_BILL2 is chosen
  IF s_matnr IS INITIAL AND s_date IS INITIAL.
    MESSAGE text-018 TYPE c_e. " You must place value on
    " Creation-Modification Date
*    LEAVE TO TRANSACTION c_tcode3.
  ENDIF.

*--data selection.
  PERFORM get_data.
*--create idoc.
  IF NOT v_lines IS INITIAL.
    PERFORM create_idoc.
  ELSE.
    MESSAGE text-017 TYPE 'I'.
    LEAVE LIST-PROCESSING.

  ENDIF.

END-OF-SELECTION.

*----------------------------------------------------------------------*
*        LIST PROCESSING                                               *
*----------------------------------------------------------------------*
*  Output/Display the report after processing.                         *
*----------------------------------------------------------------------*
*--display top of page.
  PERFORM top_of_page .
*--display output.
  PERFORM display_output.

*----------------------------------------------------------------------*
*        SUBROUTINES                                                   *
*----------------------------------------------------------------------*
*  Subroutines declaration                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data                                                 *
*&---------------------------------------------------------------------*
FORM get_data.
  DATA: v_object LIKE cdhdr-objectid.

* select material by plant
  SELECT matnr
         werks
         mmsta
         lvorm
         mmstd
         ausme
         beskz
         sobsl
         rgekz
         kausf
         nfmat
  FROM marc
  INTO TABLE i_marc
  WHERE werks IN s_werks
  AND   matnr IN s_matnr
  AND   mmsta IN s_mmsta.

* Delete material not include in material typeselect material by plant

  IF NOT i_marc[] IS INITIAL.
    SELECT matnr
           mtart
           ean11
           meins
           bstme
           brgew
           ntgew
           gewei
           volum
           voleh
    FROM   mara
    INTO TABLE i_mara
    FOR ALL ENTRIES IN i_marc
    WHERE matnr      = i_marc-matnr
    AND   mtart     IN s_mtart.


* Delete from i_marc material with not material type in selection screen
    LOOP AT i_marc.
      READ TABLE i_mara WITH KEY matnr = i_marc-matnr.
      IF sy-subrc <> 0.
        DELETE i_marc WHERE matnr = i_marc-matnr.
      ENDIF.
    ENDLOOP.

  ENDIF.

*--BEGIN OF MODIFICATION: PRILANO 07/26/2006 DA2K926199
*--Replace constants with text-elements.

  IF NOT i_marc[] IS INITIAL.
    REFRESH: i_cdhdr.

    LOOP AT i_marc.
      SELECT  objectclas
                objectid
                changenr
                tcode
                udate
                change_ind
      FROM cdhdr
      APPENDING TABLE i_cdhdr
      WHERE udate IN s_date
*  AND ( objectclas EQ c_classify OR
*        objectclas EQ c_material )
  AND ( objectclas EQ text-t02 OR
        objectclas EQ text-t01 )
  AND   objectid = i_marc-matnr
*  AND ( tcode      EQ c_mm01     OR
*        tcode      EQ c_mm02     OR
*        tcode      EQ c_mm06 ).
  AND ( tcode      EQ text-t03     OR
        tcode      EQ text-t04     OR
        tcode      EQ text-t05 ).
    ENDLOOP.
  ENDIF.

*--END OF MODIFICATION: PRILANO 07/26/2006 DA2K926199
*--Replace constants with text-elements.

*--Look for the fields that have been changed in the interface.
  IF NOT i_cdhdr[] IS INITIAL.
    SELECT changenr
           tabname
           fname
           chngind
           objectid
    FROM   cdpos
    INTO TABLE i_cdpos
    FOR ALL ENTRIES IN i_cdhdr
    WHERE objectclas = i_cdhdr-objectclas AND
          objectid = i_cdhdr-objectid AND
          changenr  = i_cdhdr-changenr AND
          tabname  IN r_tabname.
*    AND   fname    IN r_fname.

    IF sy-subrc EQ 0.
    ENDIF.

    CLEAR: x_cdpos.
    LOOP AT i_cdpos INTO x_cdpos
          WHERE chngind <> 'I'.
      IF NOT x_cdpos-fname IN r_fname.
        DELETE i_cdpos.
        IF sy-subrc EQ 0.
        ENDIF.
      ENDIF.

      CLEAR: x_cdpos.
    ENDLOOP.

*  Records changed or created
    LOOP AT i_cdhdr INTO x_cdhdr
        WHERE change_ind <> 'I'.  "change
      READ TABLE i_cdpos INTO x_cdpos
            WITH KEY  objectid = x_cdhdr-objectid.
      IF sy-subrc <> 0.
        DELETE i_cdhdr WHERE objectid = x_cdhdr-objectid.
      ENDIF.
    ENDLOOP.
  ENDIF.

*--get product description.
  IF NOT i_marc[] IS INITIAL.
    SELECT matnr
           maktx
    FROM makt
    INTO TABLE i_makt
    FOR ALL ENTRIES IN i_marc
    WHERE  matnr = i_marc-matnr.

    IF sy-subrc EQ 0.
    ENDIF.
  ENDIF.

*--get Units of Measure for Material
  IF NOT i_marc[] IS INITIAL.
    SELECT matnr
           meinh
           umrez
           umren
    FROM marm
    INTO TABLE i_marm
    FOR ALL ENTRIES IN i_marc
    WHERE matnr = i_marc-matnr.

    IF sy-subrc EQ 0.
    ENDIF.
  ENDIF.


*{begin of change guamosch MUSIC 06/22/2006 DA2K925080
*ADD code to get Maturing Indicator and Maturing Plant

*--get Maturing indicator and Maturing plant

** MATURING_INDICATOR
  CLEAR v_latinn.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = c_mat_indict
    IMPORTING
      output = v_latinn.

  IF sy-subrc EQ 0.
  ENDIF.

  IF sy-subrc = 0.

    IF NOT i_marc[] IS INITIAL.

      REFRESH: i_ausp.
      CLEAR: x_marc.
      LOOP AT i_marc INTO x_marc.

        SELECT objek
    	        atinn
    	        atwrt
               FROM ausp
               APPENDING TABLE i_ausp
               WHERE objek = x_marc-matnr AND
                     atinn = v_latinn.

        IF sy-subrc EQ 0.
        ENDIF.

        CLEAR: x_marc.
      ENDLOOP.
      	

ENDIF.
      		

ENDIF.
      		
** MATURING PLANT
      CLEAR v_latinn.
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = c_mat_plant
        IMPORTING
          output = v_latinn.

      IF sy-subrc EQ 0.
      ENDIF.

      IF sy-subrc = 0.

        IF NOT i_marc[] IS INITIAL.

          REFRESH: i_ausp2.
          CLEAR: x_marc.
          LOOP AT i_marc INTO x_marc.

            SELECT objek
        	     atinn
        	     atwrt
                   FROM ausp
                   APPENDING TABLE i_ausp2
                   WHERE objek = x_marc-matnr AND
                         atinn = v_latinn.

            IF sy-subrc EQ 0.
            ENDIF.

            CLEAR: x_marc.
          ENDLOOP.


        ENDIF.
        	     	
      ENDIF.

*}end of change guamosch MUSIC 06/22/2006 DA2K925080

*--get the list of item material master
        CLEAR: x_mara,
               x_marc,
               x_makt,
               x_marm,
               x_output.

        SORT i_mara BY matnr.
        LOOP AT i_marc INTO x_marc.
          READ TABLE i_cdhdr INTO x_cdhdr
                  WITH KEY  objectid = x_marc-matnr.
          IF sy-subrc = 0.

            CASE x_cdhdr-tcode.
              WHEN 'MM01'.
                x_output-msgfn = '009'.
              WHEN 'MM02'.
                x_output-msgfn = '004'.
              WHEN 'MM06'.
                x_output-msgfn = '003'.
            ENDCASE.

            MOVE: x_marc-lvorm  TO x_output-lvorm,
                   x_marc-werks  TO x_output-werks,
                   x_marc-mmsta  TO x_output-mmsta,
                   x_marc-mmstd  TO x_output-mmstd,
                   x_marc-ausme  TO x_output-ausme,
                   x_marc-beskz  TO x_output-beskz,
                   x_marc-sobsl  TO x_output-sobsl,
                   x_marc-rgekz  TO x_output-rgekz,
                   x_marc-kausf  TO x_output-kausf,
                   x_marc-nfmat  TO x_output-nfmat.
            CLEAR x_mara.
            READ TABLE i_mara INTO x_mara WITH KEY matnr = x_marc-matnr.
            IF sy-subrc = 0.
              MOVE:  x_mara-matnr  TO x_output-matnr,
                     x_mara-mtart  TO x_output-mtart,
                     x_mara-ean11  TO x_output-ean11,
                     x_mara-meins  TO x_output-meins,
                     x_mara-bstme  TO x_output-bstme,
                     x_mara-brgew  TO x_output-brgew,
                     x_mara-ntgew  TO x_output-ntgew,
                     x_mara-gewei  TO x_output-gewei,
                     x_mara-volum  TO x_output-volum,
                     x_mara-voleh  TO x_output-voleh.

            ENDIF.

            CLEAR: x_makt.
            READ TABLE i_makt INTO x_makt WITH KEY matnr = x_marc-matnr.
            IF sy-subrc EQ 0.
              MOVE: x_makt-maktx  TO x_output-maktx.
            ENDIF.

*    CLEAR: x_marm.
*    READ TABLE i_marm INTO x_marm WITH KEY matnr = x_marc-matnr.
*    IF sy-subrc EQ 0.
*      MOVE: x_marm-meinh  TO x_output-meinh,
*            x_marm-umrez  TO x_output-umrez,
*            x_marm-umren  TO x_output-umren.
*    ENDIF.

            APPEND x_output TO i_output.
            CLEAR: x_mara,
               x_marc,
               x_makt,
               x_marm,
               x_output.
          ENDIF.
        ENDLOOP.

*--count number of lines
        CLEAR v_lines.

        SORT i_output BY mtart matnr.
        DELETE ADJACENT DUPLICATES FROM i_output.

        DESCRIBE TABLE i_output LINES v_lines.
*  CLEAR: x_output,
*         x_counter.
*  LOOP AT i_output INTO x_output.
*    MOVE x_output-matnr   TO x_counter-matnr.
*    MOVE x_output-counter TO x_counter-counter.
*
*    IF sy-subrc EQ 0.
*      COLLECT x_counter INTO i_counter.
*    ENDIF.
*  ENDLOOP.

***}START OF INSERT MATEOJA 10/19/07 R11 UNITY DEFECT 3598 CD0K912823
        CLEAR x_output.
        LOOP AT i_output INTO x_output.
* retrieve partner form T9CON using plant
          CLEAR v_rcvprn.
          SELECT SINGLE result
              FROM t9con
              INTO  v_rcvprn
              WHERE key1 = c_mf AND
                    key2 = text-t01 AND
                    key3 = x_output-werks.

          IF sy-subrc EQ 0.
          ENDIF.

* Check if Partner is metric or imperial
          CLEAR v_zmeins.
          SELECT SINGLE zmeins
            INTO v_zmeins
            FROM zplant_partners
           WHERE werks EQ x_output-werks.
* start of modificatin 06/12/2007 sanchema defect 542
*             AND rcvprn EQ v_rcvprn.
* end of modification 06/12/2007 sanchema defect 542
          IF sy-subrc EQ 0.
            CASE v_zmeins.
              WHEN c_01.   "Imperial partner
                CLEAR: v_old_unit, v_old_value, v_new_value, v_new_unit,
                       v_conversion.
                v_conversion = c_01.
                v_old_unit = x_output-gewei.
                v_old_value = x_output-brgew.
                PERFORM check_unit USING x_output-gewei
                                         v_conversion
                                CHANGING v_new_unit.

                PERFORM unit_convert USING v_old_value
                                           v_old_unit
                                           v_new_unit
                                  CHANGING v_new_value.
                CLEAR x_output-brgew.
                x_output-brgew = v_new_value.

                CLEAR: v_old_unit, v_old_value, v_new_value.
                v_old_value = x_output-ntgew.
                PERFORM unit_convert USING v_old_value
                                           v_old_unit
                                           v_new_unit
                                  CHANGING v_new_value.
                CLEAR: x_output-ntgew,x_output-gewei.
                x_output-ntgew = v_new_value.
                x_output-gewei = v_new_unit.

                CLEAR: v_old_unit, v_old_value, v_new_value, v_new_unit.
                v_old_unit = x_output-voleh.
                v_old_value = x_output-volum.
                PERFORM check_unit USING x_output-voleh
                                         v_conversion
                                CHANGING v_new_unit.

                PERFORM unit_convert USING v_old_value
                                           v_old_unit
                                           v_new_unit
                                  CHANGING v_new_value.
                CLEAR: x_output-volum, x_output-voleh.
                x_output-volum = v_new_value.
                x_output-voleh = v_new_unit.

                MODIFY i_output from x_output
                TRANSPORTING ntgew
                             brgew
                             gewei
                             volum
                             voleh
                        where
                        msgFn = x_output-msgfn and
                        mtart = x_output-mtart and
                        matnr = x_output-matnr.
                             .
              WHEN c_02.   "Metric partner
                CLEAR: v_old_unit, v_old_value, v_new_value, v_new_unit,
                       v_conversion.
                v_conversion = c_02.
                v_old_unit = x_output-gewei.
                v_old_value = x_output-brgew.
                PERFORM check_unit USING x_output-gewei
                                         v_conversion
                                CHANGING v_new_unit.

                PERFORM unit_convert USING v_old_value
                                           v_old_unit
                                           v_new_unit
                                  CHANGING v_new_value.
                CLEAR: x_output-brgew, x_output-gewei.
                x_output-brgew = v_new_value.
                x_output-gewei = v_new_unit.

                CLEAR: v_old_unit, v_old_value, v_new_value.
                v_old_unit = x_output-gewei.
                v_old_value = x_output-ntgew.
                PERFORM unit_convert USING v_old_value
                                           v_old_unit
                                           v_new_unit
                                  CHANGING v_new_value.
                CLEAR x_output-ntgew.
                x_output-ntgew = v_new_value.

                CLEAR: v_old_unit, v_old_value, v_new_value, v_new_unit.
                v_old_unit = x_output-voleh.
                v_old_value = x_output-volum.
                PERFORM check_unit USING x_output-voleh
                                         v_conversion
                                CHANGING v_new_unit.

                PERFORM unit_convert USING v_old_value
                                           v_old_unit
                                           v_new_unit
                                  CHANGING v_new_value.
                CLEAR: x_output-volum, x_output-voleh.
                x_output-volum = v_new_value.
                x_output-voleh = v_new_unit.

                MODIFY i_output from x_output
                TRANSPORTING ntgew
                             brgew
                             gewei
                             volum
                             voleh
                    where
                        msgfn = x_output-msgfn and
                        mtart = x_output-mtart and
                        matnr = x_output-matnr.

              WHEN OTHERS.
            ENDCASE.
          ENDIF.
        ENDLOOP.

***{END OF INSERT MATEOJA 10/19/07 R11 UNITY DEFECT 3598 CD0K912823



 ENDFORM.                                                      "get_data
*&---------------------------------------------------------------------*
*&      Form  create_idoc                                              *
*&---------------------------------------------------------------------*
FORM create_idoc.
*--idoc status read.
  REFRESH: i_statxt,
           i_status.
  CALL FUNCTION 'IDOC_STATUS_VALUES_READ'
    TABLES
      values         = i_statxt
    EXCEPTIONS
      internal_error = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*--Create a data segment for each line
  SORT i_output BY mtart matnr.
  CLEAR: x_output.
  REFRESH: i_edidd.
  LOOP AT i_output INTO x_output2.

*--BEGIN OF MODIFICATION: PRILANO 07/26/2006 DA2K926199
*--Replace constants with text-elements.

    MOVE: x_output2 TO x_output.

    CLEAR: x_control_rec.
* retrieve partner form T9CON usin plant
    SELECT SINGLE result
        FROM t9con
        INTO  x_control_rec-rcvprn
        WHERE key1 = c_mf AND
*              key2 = c_material AND
              key2 = text-t01 AND
              key3 = x_output-werks.

    IF sy-subrc EQ 0.
    ENDIF.

*--END OF MODIFICATION: PRILANO 07/26/2006 DA2K926199
*--Replace constants with text-elements.

* Create IDoc control record
    x_control_rec-mandt  = sy-mandt.
    x_control_rec-direct = 1.
    x_control_rec-idoctp = text-i01.
    x_control_rec-mestyp = text-i02.
    x_control_rec-rcvprt = text-i03.

    CLEAR: x_idoc_data,
           x_e1maram.
* Create IDoc data segments (mara)
    x_idoc_data-segnam  = c_e1maram.
    x_e1maram-mtart = x_output-mtart.
    x_e1maram-msgfn = x_output-msgfn.
    x_e1maram-matnr = x_output-matnr.
    x_e1maram-ean11 = x_output-ean11.
    x_e1maram-meins = x_output-meins.
    x_e1maram-bstme = x_output-bstme.
    x_e1maram-brgew = x_output-brgew.
    x_e1maram-ntgew = x_output-ntgew.
    x_e1maram-gewei = x_output-gewei.
    x_e1maram-volum = x_output-volum.
    x_e1maram-voleh = x_output-voleh.

*{begin of change guamosch MUSIC 06/22/2006 DA2K925080
*--get maturing indicator

    CLEAR: x_ausp.
    READ TABLE i_ausp INTO x_ausp
    WITH KEY objek = x_output-matnr.

    IF sy-subrc EQ 0.
      x_e1maram-przus = x_ausp-atwrt.
    ENDIF.

*}end of change guamosch MUSIC 06/22/2006 DA2K925080

    x_idoc_data-sdata   = x_e1maram.
    APPEND x_idoc_data TO i_idoc_data.

    CLEAR: x_idoc_data,
           x_e1maktm.
* Create IDoc data segments (makt)
    x_idoc_data-segnam   = c_e1maktm.
    x_e1maktm-maktx = x_output-maktx.
    x_idoc_data-sdata   = x_e1maktm.
    APPEND x_idoc_data TO i_idoc_data.

    CLEAR: x_idoc_data,
           x_e1marcm.
* Create IDoc data segments (marc)
    x_idoc_data-segnam  = c_e1marcm.
    x_e1marcm-lvorm = x_output-lvorm.
    x_e1marcm-werks = x_output-werks.
    x_e1marcm-mmsta = x_output-mmsta.
    x_e1marcm-mmstd = x_output-mmstd.
    x_e1marcm-ausme = x_output-ausme.
    x_e1marcm-beskz = x_output-beskz.
    x_e1marcm-sobsl = x_output-sobsl.
    x_e1marcm-rgekz = x_output-rgekz.
    x_e1marcm-kausf = x_output-kausf.
    x_e1marcm-nfmat = x_output-nfmat.
    x_idoc_data-sdata   = x_e1marcm.
    APPEND x_idoc_data TO i_idoc_data.


    LOOP AT i_marm INTO x_marm
        WHERE matnr = x_output-matnr.
      CLEAR: x_idoc_data,
             x_e1marmm.
* Create IDoc data segments (marm)
      x_idoc_data-segnam  = c_e1marmm.
      x_e1marmm-meinh = x_marm-meinh.
      x_e1marmm-umrez = x_marm-umrez.
      x_e1marmm-umren = x_marm-umren.
      x_idoc_data-sdata   = x_e1marmm.
      APPEND x_idoc_data TO i_idoc_data.
    ENDLOOP.


*{begin of change guamosch MUSIC 06/22/2006 DA2K925080
*--get maturing plant

    CLEAR: x_ausp2.
    LOOP AT i_ausp2 INTO x_ausp2
    WHERE objek = x_output-matnr.

      CLEAR: x_idoc_data,
             x_e1mtxhm.

* Create IDoc data segments
      x_idoc_data-segnam  = c_e1mtxhm.
      x_e1mtxhm-tdobject  = x_ausp2-atwrt.
      x_idoc_data-sdata   = x_e1mtxhm.
      APPEND x_idoc_data TO i_idoc_data.

      CLEAR: x_ausp2.
    ENDLOOP.


*}end of change guamosch MUSIC 06/22/2006 DA2K925080


*    AT END OF mtart.

*--Call the IDoc creation function
    REFRESH: i_edidc.
    CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
      EXPORTING
        master_idoc_control        = x_control_rec
      TABLES
        communication_idoc_control = i_edidc
        master_idoc_data           = i_idoc_data.
*      EXCEPTIONS
*        error_in_idoc_control          = 1
*        error_writing_idoc_status      = 2
*        error_in_idoc_data             = 3
*        sending_logical_system_unknown = 4
*        OTHERS                         = 5.


    IF sy-subrc <> 0.
    ENDIF.
** start 26-07-2006  sanchema defect 3143
    COMMIT WORK.
** end 26-07-2006  sanchema defect 3143
    CLEAR: x_edidc,
           x_status.
    READ TABLE i_edidc INTO x_edidc INDEX 1.

    IF sy-subrc EQ 0.

      CLEAR: x_statxt.
      READ TABLE i_statxt INTO x_statxt
      WITH KEY value = x_edidc-status.

      IF sy-subrc EQ 0.

        MOVE: x_statxt-descrp TO x_status-txt.

      ENDIF.

      MOVE: x_output-matnr TO x_status-matnr,
            x_edidc-status TO x_status-status,
            x_edidc-docnum TO x_status-docnum.

    ENDIF.

    REFRESH: i_idoc_data.

*    ENDAT.

    APPEND x_status TO i_status.

    CLEAR: x_output,
           x_output2,
           x_status.
  ENDLOOP.



ENDFORM.                                                    "create_idoc
*&---------------------------------------------------------------------*
*&      Form  display_output                                           *
*&---------------------------------------------------------------------*
FORM display_output.
*--BEGIN OF MODIFICATION: PRILANO 07/26/2006 DA2K926199
*--Replace constants with text-elements.

*--show selection Screen data
  WRITE:/.
*  WRITE:/5  c_sldat.
  WRITE:/5  text-t06.
  WRITE:/10(60) sy-uline.

  WRITE:/10  c_plant.
  WRITE: 60  c_to.
  FORMAT COLOR 3 ON.
  WRITE: 42 s_werks-low.
  WRITE: 60 s_werks-high.
  FORMAT COLOR OFF.

  WRITE:/10  text-003.
  WRITE: 60  c_to.
  FORMAT COLOR 3 ON.
  WRITE: 42 s_matnr-low.
  WRITE: 60 s_matnr-high.
  FORMAT COLOR OFF.

*  WRITE:/10  c_mtart.
  WRITE:/10  text-t07.
  WRITE: 60  c_to.
  FORMAT COLOR 3 ON.
  WRITE: 42 s_mtart-low.
  WRITE: 60 s_mtart-high.
  FORMAT COLOR OFF.

*  WRITE:/10  c_mmsta.
  WRITE:/10  text-t08.
  WRITE: 60  c_to.
  FORMAT COLOR 3 ON.
  WRITE: 42 s_mmsta-low.
  WRITE: 60 s_mmsta-high.
  FORMAT COLOR OFF.

*  WRITE:/10  c_date1.
  WRITE:/10  text-t09.
  WRITE: 60  c_to.
  FORMAT COLOR 3 ON.
  WRITE: 42 s_date-low.
  WRITE: 60 s_date-high.
  FORMAT COLOR OFF.


*  WRITE:/10  c_matsend.
**  IF rb_all EQ c_x.
**    WRITE: 42 c_all COLOR 3.
**  ELSE.
*    WRITE: 42 c_mod COLOR 3.
**  ENDIF.
  WRITE:/10(60) sy-uline.
  WRITE:/.

*--number of materials selected

*  WRITE:/40 v_lines COLOR 4,5  c_matselec.
  WRITE:/40 v_lines COLOR 4,5  text-t13.
  WRITE:/.
  ULINE.
  FORMAT COLOR 1 ON.
  WRITE:/60 c_logxt,
         142 space.
  FORMAT COLOR OFF.
  ULINE.
  WRITE:/.

  FORMAT COLOR 1 ON.
*  WRITE:/10 c_material2, 30 c_idocnum, 50 c_status,
  WRITE:/10 text-t18, 30 text-t16, 50 text-t17,
         142 space.
  FORMAT COLOR OFF.
  SKIP.
  SKIP.

  FORMAT COLOR 5 ON.
*  WRITE:/10  c_sucpr,
  WRITE:/10  text-t14,
         142 space.
  FORMAT COLOR OFF.
  WRITE:/.

*--END OF MODIFICATION: PRILANO 07/26/2006 DA2K926199
*--Replace constants with text-elements.

* Looping through I_status to output the records with Success status

  DELETE i_status WHERE matnr IS INITIAL.

  IF sy-subrc EQ 0.
  ENDIF.

  CLEAR x_status.
  LOOP AT i_status INTO x_status.
    v_txt = x_status-txt.
    TRANSLATE v_txt TO UPPER CASE.                       "#EC TRANSLANG

    IF x_status-status = c_03.

      WRITE:/10 x_status-matnr, 30 x_status-docnum, 50 x_status-txt.

    ENDIF.
    IF x_status-status = c_30.

      WRITE:/10 x_status-matnr, 30 x_status-docnum, 50 x_status-txt.

    ENDIF.

    CLEAR x_status.
    CLEAR v_txt.
  ENDLOOP.
  .
  WRITE:/.
*--Begin OF MODIFICATION: PRILANO 07/26/2006 DA2K926199
*--Replace constants with text-elements.

  FORMAT COLOR 6 ON.
*  WRITE:/10  c_errpr,
  WRITE:/10  text-t15,
         142 space.
  FORMAT COLOR OFF.
  WRITE:/.
*--END OF MODIFICATION: PRILANO 07/26/2006 DA2K926199
*--Replace constants with text-elements.

* Looping through I_status to output the records with ERROR status

  CLEAR x_status.
  LOOP AT i_status INTO x_status.
    v_txt = x_status-txt.
    TRANSLATE v_txt TO UPPER CASE.                       "#EC TRANSLANG

*   stat other than 03 for outbound is Error
    IF x_status-status <> c_03 AND
       x_status-status <> c_30.

      WRITE:/10 x_status-matnr, 30 x_status-docnum, 50 x_status-txt.

    ENDIF.
    CLEAR x_status.
    CLEAR v_txt.
  ENDLOOP.
ENDFORM.                                                    "create_idoc

*&---------------------------------------------------------------------*
*&      Form  add_range                                                *
*&---------------------------------------------------------------------*
FORM add_range USING p_field.

  MOVE: c_i      TO r_fname-sign,
        c_eq     TO r_fname-option,
        p_field  TO r_fname-low.
  APPEND r_fname.

ENDFORM.                                                     "add_range
*&---------------------------------------------------------------------*
*&      Form  add_table                                                *
*&---------------------------------------------------------------------*
FORM add_table USING p_table.

  MOVE: c_i      TO r_tabname-sign,
        c_eq     TO r_tabname-option,
        p_table  TO r_tabname-low.
  APPEND r_tabname.

ENDFORM.                                                     "add_table
*----------------------------------------------------------------------*
*        TOP-OF-PAGE                                                   *
*----------------------------------------------------------------------*
*  This event occurs as soon as the system starts processing a new     *
*  page of a list. The system processes the statements following       *
*  TOP-OF-PAGE before outputting the first line on a new page.         *
*----------------------------------------------------------------------*
FORM top_of_page .

  ULINE.
  WRITE:  5 sy-repid, 130 sy-uname.
  WRITE:/5 sy-datum, 20 sy-uzeit, 132 sy-pagno, 130 c_page.
  ULINE.



ENDFORM.                                                   " TOP_OF_PAG
*&---------------------------------------------------------------------*
*&      Form  unit_convert
*&---------------------------------------------------------------------*
FORM unit_convert  USING v_lold_value
                         v_lold_unit
                CHANGING v_lnew_unit
                         v_lnew_value.

  CALL FUNCTION 'CF_UT_UNIT_CONVERSION'
    EXPORTING
      unit_new_imp  = v_lnew_unit
      unit_old_imp  = v_lold_unit
      value_old_imp = v_lold_value
    IMPORTING
      value_new_exp = v_lnew_value
    EXCEPTIONS
      overflow      = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " unit_convert
*&---------------------------------------------------------------------*
*&      Form  check_unit
*&---------------------------------------------------------------------*
FORM check_unit  USING    v_loutput
                          v_lconversion
                 CHANGING v_lnew_unit.

  DATA: v_ltemp_unit TYPE char5.
  CLEAR v_ltemp_unit.

  IF v_lconversion EQ c_01.
* Check if UoM needs to be converted
    SELECT SINGLE zimperial
      INTO v_ltemp_unit
      FROM ztmetric_imp_cv
     WHERE zimperial EQ v_loutput.

    IF sy-subrc EQ 0.
      v_lnew_unit = v_loutput.
    ELSE.
      SELECT SINGLE zimperial
        FROM ztmetric_imp_cv
        INTO v_lnew_unit
       WHERE zmetriz EQ v_old_unit.

      IF sy-subrc EQ 0.
      ENDIF.
    ENDIF.
  ELSE.
* Check if UoM needs to be converted
    SELECT SINGLE zmetriz
      INTO v_ltemp_unit
      FROM ztmetric_imp_cv
     WHERE zmetriz EQ v_loutput.

    IF sy-subrc EQ 0.
      v_lnew_unit = v_loutput.
    ELSE.
      SELECT SINGLE zmetriz
        FROM ztmetric_imp_cv
        INTO v_lnew_unit
       WHERE zimperial EQ v_old_unit.

      IF sy-subrc EQ 0.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_unit
