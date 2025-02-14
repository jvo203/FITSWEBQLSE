#include <glib.h>
#include <math.h>

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

extern void begin_json_array(GString *json, char *key)
{
    if (json == NULL)
        return;

    g_string_append_printf(json, "\"%s\" : [", key);
}

extern void end_json_array(GString *json, int count)
{
    if (json == NULL)
        return;

    if (count > 0)
    {
        // remove the final ","
        g_string_truncate(json, json->len - 1);
    }

    // end array
    g_string_append_printf(json, "],");
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

extern void add_json_float_with_precision(GString *json, char *key, float val, int precision)
{
    if (json == NULL)
        return;

    if (!isnan(val))
        g_string_append_printf(json, "\"%s\" : %.*f,", key, precision, val);
    else
        g_string_append_printf(json, "\"%s\" : null,", key);
}

extern void add_json_float(GString *json, char *key, float val)
{
    add_json_float_with_precision(json, key, val, 16);

    /*if (json == NULL)
        return;

    if (!isnan(val))
        g_string_append_printf(json, "\"%s\" : %.16g,", key, val);
    else
        g_string_append_printf(json, "\"%s\" : null,", key);*/
}

extern void add_json_double(GString *json, char *key, double val)
{
    if (json == NULL)
        return;

    if (!isnan(val))
        g_string_append_printf(json, "\"%s\" : %.32g,", key, val);
    else
        g_string_append_printf(json, "\"%s\" : null,", key);
}

extern void add_json_integer_array(GString *json, char *key, int *val, int n)
{
    int i;

    if (json == NULL)
        return;

    g_string_append_printf(json, "\"%s\" : [", key);

    if (n > 0)
    {
        for (i = 0; i < n; i++)
            g_string_append_printf(json, "%d,", val[i]);

        // remove the final ","
        g_string_truncate(json, json->len - 1);
    }

    g_string_append_printf(json, "],");
}

extern void add_json_logical(GString *json, char *key, bool val)
{
    if (json == NULL)
        return;

    g_string_append_printf(json, "\"%s\" : %s,", key, val ? "true" : "false");
}

extern void add_json_polarisation_entry(GString *json, float x, float y, float intensity, float angle)
{
    if (json == NULL)
        return;

    g_string_append_printf(json, "{\"x\" : %.2g, \"y\" : %.2g, \"I\" : %.16g, \"A\" : %.16g},", x, y, intensity, angle);
}