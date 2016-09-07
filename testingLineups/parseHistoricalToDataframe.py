import csv

with open('data_warehouse/2015_week_4.txt', 'r') as file:
    stripped = (line.strip() for line in file)
    lines = (line for line in stripped if line)
    grouped = zip(*[lines] * 13)
    with open('data_warehouse/cleaned_2015_week_4.csv', 'w') as out_file:
        writer = csv.writer(out_file)
        writer.writerow(('Position', 'Name', 'Team', 'Salary', 'ProductionPoints', ' ProjectedPoints', 'Depth', 'RandomStat', 'AvgFFPG', 'ProdValue', 'ProjValue', 'Opp', 'OPRK'))
        writer.writerows(grouped)