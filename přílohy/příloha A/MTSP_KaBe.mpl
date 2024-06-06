TITLE MTSP_KaBe;

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
K=2;

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

vjezdy[j>=2]:   sum(i:x[i,j]) = 1;
vyjezdy[i>=2]:  sum(j:x[i,j]) = 1;

hornimez[i>=2]:  u[i] + x[1,j:=i]*(L-2) - x[i,1] <= L-1;
dolnimez[i>=2]:  u[i] + x[1,j:=i] + x[i,1]*(2-K) >= 2;
doplnek[i>=2]:   x[1,j:=i] + x[i,1] <= 1;

anticykl[i,j>=2]: u[i]-u[i:=j] + L*x[i,j] + x[i:=j,j:=i]*(L-2) <= L-1;

END