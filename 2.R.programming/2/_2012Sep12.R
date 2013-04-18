molar_solution = function(x,y){
  58.433 * x * y / 10^6
}

molar_solution2 = function(x,y, FW){
  FW * x * y / 10^6
}

molar_solution( 1000, 1000)
concentration = 100 #mM
volume = 500 #mL
NaCl = molar_solution( concentration, volume)
NaOH = molar_solution2( concentration, volume, 40)
molar_solution2( 1000, 1000, 40)
molar_solution2( 100, 634, 74)

