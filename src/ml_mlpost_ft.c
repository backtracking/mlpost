

#include <assert.h>
#include <string.h>

#define CAML_NAME_SPACE

#include <caml/mlvalues.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#define FT_Face_val(v) (FT_Face)(Field(v, 0))

CAMLprim value
ml_FT_Get_Name_Index(value font, value char_name)
{
  int index = FT_Get_Name_Index (FT_Face_val (font),
                                 String_val (char_name));
  return Val_int (index);
}


CAMLprim value
ml_FT_Get_Char_Index(value font, value charcode)
{
  int index = FT_Get_Char_Index (FT_Face_val (font),
                                 Long_val (charcode));
  return Val_int (index);
}

CAMLprim value
ml_FT_num_charmaps(value font)
{
    FT_Face face = FT_Face_val (font);

    return Val_int (face->num_charmaps);
}

CAMLprim value
ml_FT_set_charmap(value font, value charmap_index)
{
    FT_Face face = FT_Face_val (font);
    FT_CharMap charmap = (face->charmaps)[Int_val(charmap_index)];

    return Val_int (FT_Set_Charmap(face,charmap));
}
