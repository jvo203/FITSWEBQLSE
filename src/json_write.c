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
    if (json != NULL)
    {
        // remove the final ","
        g_string_truncate(json, json->len - 1);

        // end JSON
        g_string_printf(json, "}");
    }
}

void add_json_string(GString *json, char *key, char *val)
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

void add_json_integer(GString *json, char *key, int val)
{
    if (json == NULL)
        return;

    g_string_append_printf(json, "\"%s\" : %d,", key, val);
}