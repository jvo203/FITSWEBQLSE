#include <stdlib.h>
#include <stdio.h>
#include <star/ast.h>

void test_fk4()
{
    double ra1, dec1, ra2, dec2;

    astBegin;

    AstSkyFrame *fk4 = astSkyFrame("System=fk4");
    AstSkyFrame *icrs = astSkyFrame("System=ICRS");

    AstFrameSet *fk42icrs = astConvert(fk4, icrs, " ");

    // ds9 ra,dec in FK4
    ra1 = 51.4924676 * AST__DD2R;
    dec1 = 31.0959144 * AST__DD2R;

    printf("Original ra,dec (ds9) exported as FK4: %f %f\n", ra1 * AST__DR2D, dec1 * AST__DR2D);
    astTran2(fk42icrs, 1, &ra1, &dec1, 1, &ra2, &dec2);
    printf("AST FK4 --> ICRS: %f %f\n", ra2 * AST__DR2D, dec2 * AST__DR2D);

    printf("====================================================\n");

    astEnd;
}

void test_fk5()
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
    // ra1 = 248.09530913897819 * AST__DD2R;
    // dec1 = -24.476817083331749 * AST__DD2R;

    printf("Original ra,dec (ds9) exported as ICRS: %f %f\n", ra1 * AST__DR2D, dec1 * AST__DR2D);
    astTran2(icrs2fk5, 1, &ra1, &dec1, 1, &ra2, &dec2);
    printf("AST ICRS --> FK5: %f %f\n", ra2 * AST__DR2D, dec2 * AST__DR2D);

    // Clean up
    // not really needed as astEnd should do it
    astAnnul(icrs2fk5);
    astAnnul(fk52icrs);

    astAnnul(icrs);
    astAnnul(fk5);

    astEnd;
}

int main()
{
    // test_fk4();

    test_fk5();

    return 0;
}