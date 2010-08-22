

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
