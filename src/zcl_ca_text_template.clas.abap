CLASS zcl_ca_text_template DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_section_content,
        langu   TYPE sylangu,
        section TYPE zca_e_ttempl_section,
        content TYPE zca_e_ttempl_content,
      END OF ts_section_content .
    TYPES:
      tt_section_content TYPE STANDARD TABLE OF ts_section_content WITH EMPTY KEY .

    METHODS exist
      IMPORTING
        !iv_appl        TYPE zca_e_ttempl_appl
        !iv_name        TYPE zca_e_ttempl_name
        !iv_langu       TYPE sylangu OPTIONAL
      RETURNING
        VALUE(rv_exist) TYPE sap_bool .
    METHODS read
      IMPORTING
        !iv_appl  TYPE zca_e_ttempl_appl
        !iv_name  TYPE zca_e_ttempl_name
        !iv_langu TYPE sylangu OPTIONAL
      EXPORTING
        !et_data  TYPE tt_section_content .
    METHODS save
      IMPORTING
        !iv_appl TYPE zca_e_ttempl_appl
        !iv_name TYPE zca_e_ttempl_name
        !it_data TYPE tt_section_content
      RAISING
        zcx_ca_text_template .
    METHODS delete
      IMPORTING
        !iv_appl  TYPE zca_e_ttempl_appl
        !iv_name  TYPE zca_e_ttempl_name
        !iv_langu TYPE sylangu OPTIONAL.
    METHODS copy
      IMPORTING
                !iv_appl_from TYPE zca_e_ttempl_appl
                !iv_appl_to   TYPE zca_e_ttempl_appl
                !iv_name_from TYPE zca_e_ttempl_name
                !iv_name_to   TYPE zca_e_ttempl_name
                !iv_langu     TYPE sylangu OPTIONAL
      RAISING   zcx_ca_text_template.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_text_template IMPLEMENTATION.


  METHOD copy.

    " Primero se lee la plantilla de origen
    read( EXPORTING iv_appl  = iv_appl_from
                    iv_name  = iv_name_from
                    iv_langu = iv_langu
                    IMPORTING et_data  = DATA(lt_data_from) ).

    IF lt_data_from IS NOT INITIAL. " Si hay datos se graba con el nuevo nombre
      TRY.
          save( EXPORTING iv_appl = iv_appl_to
                          iv_name              = iv_name_to
                          it_data              = lt_data_from ).
        CATCH zcx_ca_text_template.
          RAISE EXCEPTION TYPE zcx_ca_text_template
            EXPORTING
              textid = zcx_ca_text_template=>error_to_copy.
      ENDTRY.

    ELSE.
      RAISE EXCEPTION TYPE zcx_ca_text_template
        EXPORTING
          textid = zcx_ca_text_template=>template_not_exist.
    ENDIF.

  ENDMETHOD.


  METHOD delete.
    IF iv_langu IS NOT INITIAL.
      DATA(lt_r_langu) = VALUE zif_ca_ttemplate_data=>tt_r_langu( ( sign = 'I' option = 'EQ' low = iv_langu ) ).
    ENDIF.

    DELETE FROM zca_t_text_templ WHERE appl = iv_appl
                                       AND name = iv_name
                                       AND langu IN lt_r_langu.
  ENDMETHOD.


  METHOD exist.

    IF iv_langu IS NOT INITIAL.
      DATA(lt_r_langu) = VALUE zif_ca_ttemplate_data=>tt_r_langu( ( sign = 'I' option = 'EQ' low = iv_langu ) ).
    ENDIF.

    SELECT SINGLE @abap_true INTO @rv_exist
           FROM zca_t_text_templ
           WHERE appl = @iv_appl
                 AND name = @iv_name
                 AND langu IN @lt_r_langu.


  ENDMETHOD.


  METHOD read.

    CLEAR et_data.

    IF iv_langu IS NOT INITIAL.
      DATA(lt_r_langu) = VALUE zif_ca_ttemplate_data=>tt_r_langu( ( sign = 'I' option = 'EQ' low = iv_langu ) ).
    ENDIF.

    SELECT langu, tsection AS section, content  INTO TABLE @et_data
           FROM zca_t_text_templ
           WHERE appl = @iv_appl
                 AND name = @iv_name
                 AND langu IN @lt_r_langu.

  ENDMETHOD.


  METHOD save.
    DATA lt_data_db TYPE STANDARD TABLE OF zca_t_text_templ.

    IF it_data IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ca_text_template
        EXPORTING
          textid = zcx_ca_text_template=>no_data_to_save.
    ENDIF.

    lt_data_db = VALUE #( FOR <wa> IN it_data ( appl = iv_appl name = iv_name
                                                langu = <wa>-langu
                                                tsection = <wa>-section
                                                content = <wa>-content )  ).

    MODIFY zca_t_text_templ FROM TABLE lt_data_db.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.

      RAISE EXCEPTION TYPE zcx_ca_text_template
        EXPORTING
          textid = zcx_ca_text_template=>error_to_save.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
