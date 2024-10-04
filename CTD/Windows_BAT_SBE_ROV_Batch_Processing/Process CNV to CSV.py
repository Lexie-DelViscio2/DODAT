import pandas as pd
import datetime
import sys

file_name = sys.argv[1]
dive_cast = sys.argv[2]

header = ["Julian_Days", "Time_Elapsed_Seconds", "Temperature_ITS-90_deg_C",
          "Salinit_PSU", "Pressure_db", "Oxygen_SBE43_mg_l", "Oxygen_SBE43_ml_l", 
          "Fluoresence_ECO-FLNTU_mg_m3", "Turbidity_ECO-FLNTU_NTU", "Depth_m_lat27-8", "Cstar_Beam_Attenuation", 
          "Cstar_Beam_Transmission", "Conductivity_S_m", "Flag"]

file = pd.read_csv(str(file_name + '.cnv'), sep=r'\s+', skiprows=533, dtype=str, names=header)

#read date time variable
line_file = open(str(file_name + '.cnv'))
lines = line_file.readlines()
date_time_read =  lines[394]
date_time_read_split = date_time_read[15:35].split(" ")

date_time_object = datetime.datetime.strptime(str(date_time_read_split[0] + " " + 
                                                 date_time_read_split[1] + " " +
                                                 date_time_read_split[2] + " " +
                                                 date_time_read_split[3]), '%b %d %Y %H:%M:%S')
line_file.close()

#New column
new_date_time = []
for index, row in file.iterrows():
    seconds = float(str(row["Time_Elapsed_Seconds"]))
    new_date_time_row = date_time_object + datetime.timedelta(seconds=seconds)
    new_date_time.append(new_date_time_row)

file['DateTimeUTC'] = pd.Series(new_date_time)
file = file[ ['DateTimeUTC'] + [ col for col in file.columns if col != 'DateTimeUTC' ] ]

file.to_csv(str(file_name + '.csv'), sep=",", index=False)

total_scans_read =  lines[363]
total_scans = total_scans_read.split("=")[1]

if(dive_cast[0:3] == "CTD"):
    dive_detail = str(dive_cast) + "\t"
else:
    dive_detail = " " + "\t"


write_file = open(str(file_name + '_metadata.txt'), 'w')
write_file.write("Scans: " + str(total_scans) + "\n")
write_file.write("DateTime Start: " + str(date_time_object) + "\n")   
write_file.write("DateTime End: " + str(file.DateTimeUTC.iat[-1]) + "\n")  
write_file.write("Dive Cast: " + str(file.DateTimeUTC.iat[-1]) + "\n")  
write_file.write("Dive ID: " + str(dive_detail) + "\n")  
write_file.write(dive_detail + str(date_time_object.strftime('%Y-%m-%d')) + "\t" + str(date_time_object.strftime('%H:%M:%S')) + "\t" + str(total_scans))                  
write_file.close()