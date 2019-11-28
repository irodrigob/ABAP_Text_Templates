class ZCX_CA_TEXT_TEMPLATE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of ERROR_TO_SAVE,
      msgid type symsgid value 'ZCA_TEXT_TEMPLATES',
      msgno type symsgno value '007',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_TO_SAVE .
  constants:
    begin of NO_DATA_TO_SAVE,
      msgid type symsgid value 'ZCA_TEXT_TEMPLATES',
      msgno type symsgno value '008',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_DATA_TO_SAVE .
  constants:
    begin of TEMPLATE_NOT_EXIST,
      msgid type symsgid value 'ZCA_TEXT_TEMPLATES',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TEMPLATE_NOT_EXIST .
  constants:
    begin of ERROR_TO_COPY,
      msgid type symsgid value 'ZCA_TEXT_TEMPLATES',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_TO_COPY .
  constants:
    begin of ERROR_CREATE_MAIL_EDITOR,
      msgid type symsgid value 'ZCA_TEXT_TEMPLATES',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_CREATE_MAIL_EDITOR .
  constants:
    begin of ERROR_TO_TRANSF_DATA_TO_EDITOR,
      msgid type symsgid value 'ZCA_TEXT_TEMPLATES',
      msgno type symsgno value '012',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_TO_TRANSF_DATA_TO_EDITOR .
  constants:
    begin of TRANSPORT_ORDER_MANDATORY,
      msgid type symsgid value 'ZCA_TEXT_TEMPLATES',
      msgno type symsgno value '013',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TRANSPORT_ORDER_MANDATORY .
  constants:
    begin of ACTION_CANCELED,
      msgid type symsgid value 'ZCA_TEXT_TEMPLATES',
      msgno type symsgno value '014',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ACTION_CANCELED .
  data MV_MSG1 type STRING .
  data MV_MSG2 type STRING .
  data MV_MSG3 type STRING .
  data MV_MSG4 type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_MSG1 type STRING optional
      !MV_MSG2 type STRING optional
      !MV_MSG3 type STRING optional
      !MV_MSG4 type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CA_TEXT_TEMPLATE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_MSG1 = MV_MSG1 .
me->MV_MSG2 = MV_MSG2 .
me->MV_MSG3 = MV_MSG3 .
me->MV_MSG4 = MV_MSG4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
