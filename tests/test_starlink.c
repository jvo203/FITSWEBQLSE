#include <stdio.h>
#include <ast.h>

int main()
{
    double ra1, dec1, ra2, dec2;

    astBegin;

    AstSkyFrame *icrs = astSkyFrame("System=ICRS"); //,Epoch=2000,equinox=J2000");
    AstSkyFrame *fk5 = astSkyFrame("System=FK5");   //,Epoch=2000,equinox=J2000");

    AstFrameSet *icrs2fk5 = astConvert(icrs, fk5, " ");
    AstFrameSet *fk52icrs = astConvert(fk5, icrs, " ");

    // ds9 ra,dec in FK5
    ra1 = 52.2656215 * AST__DD2R;
    dec1 = 31.2677022 * AST__DD2R;

    printf("Original ra,dec (ds9) exported as FK5: %f %f\n", ra1 * AST__DR2D, dec1 * AST__DR2D);
    astTran2(fk52icrs, 1, &ra1, &dec1, 1, &ra2, &dec2);
    printf("AST FK5 --> ICRS: %f %f\n", ra2 * AST__DR2D, dec2 * AST__DR2D);

    printf("====================================================\n");

    // ds9 ra,dec in ICRS
    ra1 = 52.2656094 * AST__DD2R;
    dec1 = 31.2677078 * AST__DD2R;

    printf("Original ra,dec (ds9) exported as ICRS: %f %f\n", ra1 * AST__DR2D, dec1 * AST__DR2D);
    astTran2(icrs2fk5, 1, &ra1, &dec1, 1, &ra2, &dec2);
    printf("AST ICRS --> FK5: %f %f\n", ra2 * AST__DR2D, dec2 * AST__DR2D);

    astEnd;

    return 0;
}