#include "mkl_vsl.h"
#include <stdio.h>

#define EPS 1.0E-5 /* accuracy of quantile computation */

float vsl_median(float *X, int N)
{
    int i, status;
    VSLSSTaskPtr task;
    float q_order, quants;
    float params;

    MKL_INT q_order_n;
    MKL_INT p, n, nparams, xstorage;
    int indices = 1; /* the first vector component is processed */

    /* Parameters of the task and initialization */
    p = 1;
    n = N;

    q_order_n = 1;
    xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
    params = EPS;
    nparams = VSL_SS_SQUANTS_ZW_PARAMS_N;

    q_order = 0.5f;

    /* Create a task */
    status = vslsSSNewTask(&task, &p, &n, &xstorage, X, 0, &indices);

    /* Initialize the task parameters */
    status = vslsSSEditStreamQuantiles(task, &q_order_n, &q_order, &quants, &nparams, &params);

    /* Compute the percentiles with accuracy eps */
    status = vslsSSCompute(task, VSL_SS_STREAM_QUANTS, VSL_SS_METHOD_SQUANTS_ZW);

    /* Deallocate the task resources */
    status = vslSSDeleteTask(&task);

    return quants;
}