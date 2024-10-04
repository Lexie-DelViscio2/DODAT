echo off

set path=%PATH%;C:\Program Files (x86)\Sea-Bird\SBEDataProcessing-Win32

set insConfig=Y:\II2402_SATFADS\ROVCTD\ROV_Config\SBE19plusV2_8108.xmlcon

set basePath=Y:\II2402_SATFADS\ROVCTD

set castNum=SBE19plus_01908108_2024_07_26_0021
set rovNum=CTD-ROV-21

set root=C:\Users\davies\AppData\Local\miniconda3

call %root%\Scripts\activate.bat %root%

%root%\python.exe "Y:\II2402_SATFADS\ROVCTD\Batch_ROV_CTD_Proc\Process CNV to CSV.py" "%basePath%\%rovNum%\%rovNum%" "%rovNum%"

type "%basePath%\%rovNum%\%rovNum%_metadata.txt"

echo "Completed processing of: %rovNum%"

pause



