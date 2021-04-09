#include <glib.h>

GString *begin_json()
{
    GString *json = g_string_sized_new(4096);

    if (json != NULL)
        g_string_printf(json, "{");

    return json;
}

void delete_json(GString *json)
{
    if (json != NULL)
        g_string_free(json, TRUE);
}

void end_json(GString *json)
{
    // remove the 'last ,'

    if (json != NULL)
        g_string_printf(json, "}");
}