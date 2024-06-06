TITLE MTSP;

OPTIONS
ExcelWorkBook="data.xlsx";
ExcelSheetname="vychodoceska";

INDEX
!i:=1..156;
!j:=1..156;
i:=ExcelRange("i");
j:=ExcelRange("j");

DATA
c[i,j]:=ExcelRange("data");
m=5;
L=15;

BINARY VARIABLES
!x[i,j] Export to ExcelRange("xx");
x[i,j];

INTEGER VARIABLES
u[i];


MODEL
MIN Z = sum(i,j:c[i,j]*x[i,j]);

SUBJECT TO
startvyjezdy:   sum(j>=2:x[1,j]) = m;
startvjezdy:    sum(i>=2:x[i,1]) = m;

vyjezdy[i>=2]:  sum(j:x[i,j]) = 1;
vjezdy[j>=2]:   sum(i:x[i,j]) = 1;

anticykl[i,j>=2]: u[i]-u[i:=j] + L*x[i,j] <= L-1;


END