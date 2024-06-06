TITLE TSP;

OPTIONS
ExcelWorkBook="data.xlsx";
ExcelSheetname="vychodoceska";

INDEX
i:=ExcelRange("i");
j:=ExcelRange("j");

DATA
c[i,j]:=ExcelRange("data");
n:=ExcelRange("n");

BINARY VARIABLES
x[i,j];

INTEGER VARIABLES
u[i];

MODEL
MIN Z=sum(i,j:c[i,j]*x[i,j]);

SUBJECT TO

vyjezdy[i]: sum(j:x[i,j])=1;
vjezdy[j]:  sum(i:x[i,j])=1;

hrana[i,j>=2]:u[i]+1-n*(1-x[i,j])<=u[i:=j];

END