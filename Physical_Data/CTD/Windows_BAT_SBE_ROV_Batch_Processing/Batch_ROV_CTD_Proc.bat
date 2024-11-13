echo off

set path=%PATH%;C:\Program Files (x86)\Sea-Bird\SBEDataProcessing-Win32

set insConfig=Y:\II2402_SATFADS\ROVCTD\ROV_Config\SBE19plusV2_8108.xmlcon

set basePath=Y:\II2402_SATFADS\ROVCTD

set /p castNum= Enter all DIGITS of the CTD CAST number -^>

set /p rovNum= Enter all DIGITS of the ROV STATION number (if ROV use CTD-ROV-nn if cast use Cast-nn) -^>

mkdir "%basePath%\%rovNum%"

copy "%basePath%\CTD_Offload\%castNum%.hex" "%basePath%\%rovNum%\%rovNum%.hex"

copy "%basePath%\CTD_Offload\%castNum%.xml" "%basePath%\%rovNum%\%rovNum%.xml"

start "" /wait datcnvw /s /i"%basePath%\CTD_Offload\%castNum%.hex" /p"%basePath%\Batch_ROV_CTD_Proc\DatCnv.psa" /f%rovNum%.cnv /o"%basePath%\%rovNum%" /c"%insConfig%"

start "" /wait filterw /s /i"%basePath%\%rovNum%\%rovNum%.cnv" /p"%basePath%\Batch_ROV_CTD_Proc\Filter.psa" /f%rovNum%_Filter.cnv /o"%basePath%\%rovNum%" /c"%insConfig%"

start "" /wait derivew /s /i"%basePath%\%rovNum%\%rovNum%_Filter.cnv" /p"%basePath%\Batch_ROV_CTD_Proc\Derive.psa" /f%rovNum%_Filter_Derive.cnv /o"%basePath%\%rovNum%" /c"%insConfig%"

start "" /wait binavgw /s /i"%basePath%\%rovNum%\%rovNum%_Filter_Derive.cnv" /p"%basePath%\Batch_ROV_CTD_Proc\BinAvg.psa" /f%rovNum%_Filter_Derive_BinAve60s.cnv /o"%basePath%\%rovNum%" /c"%insConfig%"

start "" /wait SeaPlotW /s /i"%basePath%\%rovNum%\%rovNum%_Filter_Derive_BinAve60s.cnv" /p"%basePath%\Batch_ROV_CTD_Proc\SeaPlotTSO.psa" /f%rovNum%_TSO.jpg /o"%basePath%\%rovNum%"

rename "%basePath%\%rovNum%\%rovNum%_Filter_Derive_BinAve60s.jpg" %rovNum%_Filter_Derive_BinAve60s_TSO.jpg

start "" /wait SeaPlotW /s /i"%basePath%\%rovNum%\%rovNum%_Filter_Derive_BinAve60s.cnv" /p"%basePath%\Batch_ROV_CTD_Proc\SeaPlotBtaTF.psa" /f%rovNum%_BtaTF.jpg /o"%basePath%\%rovNum%"

rename "%basePath%\%rovNum%\%rovNum%_Filter_Derive_BinAve60s.jpg" %rovNum%_Filter_Derive_BinAve60s_BtaTF.jpg

start "" /wait SeaPlotW /s /i"%basePath%\%rovNum%\%rovNum%_Filter_Derive_BinAve60s.cnv" /p"%basePath%\Batch_ROV_CTD_Proc\SeaPlotStP.psa" /f%rovNum%_StP.jpg /o"%basePath%\%rovNum%"

rename "%basePath%\%rovNum%\%rovNum%_Filter_Derive_BinAve60s.jpg" %rovNum%_Filter_Derive_BinAve60s_StP.jpg

set root=C:\Users\davies\AppData\Local\miniconda3

call %root%\Scripts\activate.bat %root%

%root%\python.exe "Y:\II2402_SATFADS\ROVCTD\Batch_ROV_CTD_Proc\Process CNV to CSV.py" "%basePath%\%rovNum%\%rovNum%" "%rovNum%"

type "%basePath%\%rovNum%\%rovNum%_metadata.txt"

echo "Completed processing of: %rovNum%"

pause



