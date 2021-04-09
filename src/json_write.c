#include <glib.h>

#include "json.h"

extern GString *begin_json()
{
    GString *json = g_string_sized_new(4096);

    if (json != NULL)
        g_string_printf(json, "{");

    return json;
}

extern void delete_json(GString *json)
{
    if (json != NULL)
        g_string_free(json, TRUE);
}

extern void end_json(GString *json)
{
    if (json != NULL)
    {
        // remove the final ","
        g_string_truncate(json, json->len - 1);

        // end JSON
        g_string_append_printf(json, "}");
    }
}

extern void add_json_string(GString *json, char *key, char *val)
{
    if (json == NULL)
        return;

    char *encoded = json_encode_string(val);

    if (encoded != NULL)
    {
        g_string_append_printf(json, "\"%s\" : %s,", key, encoded);

        free(encoded);
    }
}

extern void add_json_integer(GString *json, char *key, int val)
{
    if (json == NULL)
        return;

    g_string_append_printf(json, "\"%s\" : %d,", key, val);
}

extern void add_json_long(GString *json, char *key, long val)
{
    if (json == NULL)
        return;

    g_string_append_printf(json, "\"%s\" : %zd,", key, val);
}

extern void add_json_real(GString *json, char *key, float val)
{
    if (json == NULL)
        return;

    g_string_append_printf(json, "\"%s\" : %g,", key, val);
}