context("energybalance_functions")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("Qconduction_animal function works as expected", {
  expect_equal(Qconduction_animal(T_g= 293,T_b=303,d=10^-6,K=0.5,A=10^-3, proportion=0.2), 1000)
  expect_error(Qconduction_animal(T_g= 293,T_b=303,d=10^-6,K=0.5,A=10^-3, proportion=-0.2), "proportion >= 0 is not TRUE")
})

test_that("Qconduction_substrate function works as expected", {
  expect_equal(Qconduction_substrate(T_g= 293, T_b=303, D=0.01, K_g=0.3, A=10^-2, proportion=0.2),1.2)
})

test_that("Qconvection function works as expected", {
  expect_similar(Qconvection(T_a= 293, T_b= 303, H_L=10.45, A=0.0025, proportion=0.85),0.2731369)
})

test_that("heat_transfer_coefficient function works as expected", {
  expect_similar(heat_transfer_coefficient(V=0.5,D=0.05,K= 25.7 * 10^(-3), nu= 15.3 * 10^(-6), "cylinder"),9.936011)
  expect_similar(heat_transfer_coefficient(V=0.5,D=0.05,K= 25.7 * 10^(-3), nu= 15.3 * 10^(-6), "lizard_surface"),12.52186)
})

test_that("heat_transfer_coefficient_approximation function works as expected", {
  expect_similar(heat_transfer_coefficient_approximation(V=3,D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "sphere"),43.37924)
  expect_similar(heat_transfer_coefficient_approximation(V=3,D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "frog"),46.28954)
})

test_that("heat_transfer_coefficient_simple function works as expected", {
  expect_similar(heat_transfer_coefficient_simple(V=0.5,D=0.05, type = "Spotila"), 14.80412)
  expect_similar(heat_transfer_coefficient_simple(V=0.5,D=0.05, type = "Gates"), 11.03635)
})

test_that("Qradiation_absorbed function works as expected", {
  expect_equal(Qradiation_absorbed(a=0.9, A=1, psa_dir=0.4, psa_ref=0.4, S_dir=1000, S_dif=200, a_s=0.5), 612)
})

test_that("Qemitted_thermal_radiation function works as expected", {
  expect_similar(Qemitted_thermal_radiation(epsilon=0.96, A=1, psa_dir=0.4, psa_ref=0.6, T_b=303, T_g=293, T_a=298, enclosed=FALSE),78.35451)
  expect_similar(Qemitted_thermal_radiation(epsilon=0.96, A=1, psa_dir=0.4, psa_ref=0.6, T_b=303, T_g=293, T_a=298, enclosed=TRUE),17.7349)
  
})

test_that("Qevaporation function works as expected", {
  expect_similar(Qevaporation(A=0.1, T_b=293, taxon="amphibian", rho_s=0.003, rho_a=0.002, h=0.5, H=20, r_i=50),4.612476)
  expect_similar(Qevaporation(A=0.1, T_b=293, taxon="amphibian_wetskin", rho_s=0.003, rho_a=0.002, h=0.5, H=20, r_i=50),8.74552)
  expect_similar(Qevaporation(A=0.1, T_b=293, taxon="lizard"),1.043334)
  expect_similar(Qevaporation(A=0.1, T_b=290, taxon="lizard"),0.1005)
  expect_similar(Qevaporation(A=0.1, T_b=310, taxon="lizard"), 0.04695595)
})

test_that("Qevaporation errors work as expected", {
  expect_error(Qevaporation(A=-0.1, T_b=293, taxon="amphibian", rho_s=0.003, rho_a=0.002, h=0.5, H=20, r_i=50))
  expect_error(Qevaporation(A=0.1, T_b=410, taxon="amphibian", rho_s=0.003, rho_a=0.002, h=0.5, H=20, r_i=50))
  expect_error(Qevaporation(A=0.1, T_b=293, taxon="frog", rho_s=0.003, rho_a=0.002, h=0.5, H=20, r_i=50))
  expect_error(Qevaporation(A=0.1, T_b=293, taxon=c("frog","amphibian"), rho_s=0.003, rho_a=0.002, h=0.5, H=20, r_i=50))
  expect_error(Qevaporation(A=0.1, T_b=293, taxon="amphibian", rho_s=-0.003, rho_a=0.002, h=0.5, H=20, r_i=50))
  expect_error(Qevaporation(A=0.1, T_b=293, taxon="amphibian", rho_s=0.003, rho_a=-0.002, h=0.5, H=20, r_i=50))   
  expect_error(Qevaporation(A=0.1, T_b=293, taxon="amphibian", rho_s=0.003, rho_a=0.002, h=50, H=20, r_i=50)) 
  expect_error(Qevaporation(A=0.1, T_b=293, taxon="amphibian", rho_s=0.003, rho_a=0.002, h=0.5, H=-20, r_i=50)) 
  expect_error(Qevaporation(A=0.1, T_b=293, taxon="amphibian", rho_s=0.003, rho_a=0.002, h=0.5, H=20, r_i=-50)) 
})

test_that("saturation_water_vapor_pressure function works as expected", {
  expect_similar(saturation_water_vapor_pressure(T_a=20), 2216.563)
})

test_that("external_resistance_to_water_vapor_transfer function works as expected", {
  expect_equal(external_resistance_to_water_vapor_transfer(H=20), 558)
})

test_that("Qmetabolism_from_mass function expect_similar function works as expected", {
  expect_similar(Qmetabolism_from_mass(m=12,"reptile"), 0.02066024)
  expect_similar(Qmetabolism_from_mass(60000, taxon = "mammal"), 179.3483)
  expect_similar(Qmetabolism_from_mass(200, taxon="bird"), 4.484128)
})

test_that("Qmetabolism_from_mass_temp function works as expected", {
  expect_similar(Qmetabolism_from_mass_temp(m=100, T_b=303, "reptile"), 0.06282736)
  expect_similar(Qmetabolism_from_mass_temp(m=1, T_b=290, "invertebrate"), 0.0003268306)
  expect_similar(Qmetabolism_from_mass_temp(m=50, T_b=288, "amphibian"), 0.0113297)
  expect_similar(Qmetabolism_from_mass_temp(m=50, T_b=288, "bird"),0.03809967)

})

test_that("actual_vapor_pressure function works as expected", {
  expect_similar(actual_vapor_pressure(T_dewpoint=20), 2.393477)
  expect_similar(actual_vapor_pressure(26), 3.468698)
})

test_that("saturation_vapor_pressure function works as expected", {
  expect_similar(saturation_vapor_pressure(T_a=293), 2.341779)
})

test_that("expect_similar(boundary_layer_resistance function works as expected", {
  expect_similar(boundary_layer_resistance(T_a=293, e_s=2.5, e_a=2.4, elev=500, D=0.007, u=2), 0.3649484)
  expect_similar(boundary_layer_resistance(T_a=293, e_s=2.5, e_a=2.4, elev=500, D=0.007),4.302272)
  expect_error(boundary_layer_resistance(T_a=293, e_s=2.3, e_a=2.4, elev=500, D=0.007))
})

test_that("Tb_salamander_humid function works as expected", {
  expect_similar(Tb_salamander_humid(r_i = 4, r_b = 1, D = 0.01, T_a = 20, elev = 500, e_a = 2.0, e_s = 2.5, Qabs = 400,epsilon=0.96), 17.90782)
  expect_error(Tb_salamander_humid(r_i = 4, r_b = 1, D = 0.01, T_a = 20, elev = 500, e_a = 2.8, e_s = 2.5, Qabs = 400,epsilon=0.96))

})

test_that("Qthermal_radiation_absorbed function works as expected", {
  expect_similar(Qthermal_radiation_absorbed(T_a=20, T_g=25, epsilon_ground=0.97, a_longwave=0.965), 372.4115)
})

test_that("Tsoil function works as expected", {
  expect_similar(Tsoil(T_g_max=30, T_g_min=15, hour=12, depth=5), 20.65027)
})

test_that("Nusselt_number function works as expected", {
  expect_equal(Nusselt_number(H_L=20, D=0.01, K=0.5), 0.4)
})

test_that("Prandtl_number function works as expected", {
  expect_similar(Prandtl_number(c_p=29.3, mu=0.00001, K=0.5), 0.000586)
})

test_that("Reynolds_number function works as expected", {
  expect_similar(Reynolds_number(u=1, D=0.001, nu=1.2), 0.0008333333)
})

test_that("Grashof_number function works as expected", {
  expect_similar(Grashof_number(T_a=30, T_g=35, D=0.001, nu=1.2), 1.134259e-09)
})

test_that("Grashof_number_Gates function works as expected", {
  expect_similar(Grashof_number_Gates(T_a=30, T_g=35, beta=0.00367, D=0.001, nu=1.2), 1.248819e-10)
})

test_that("Nusselt_from_Reynolds function works as expected", {
  expect_similar(Nusselt_from_Reynolds(Re=5, taxon="cylinder"), 1.301952)
  expect_similar(Nusselt_from_Reynolds(Re=5, taxon="frog"), 0.7548014)
  expect_similar(Nusselt_from_Reynolds(Re=5, taxon="lizard_parallel_to_air_flow"), 0.3290317)
})

test_that("Nusselt_from_Grashof function works as expected", {
  expect_similar(Nusselt_from_Grashof(5), 0.7177674)
})

test_that("free_or_forced_convection function works as expected", {
  expect_identical(free_or_forced_convection(Gr=100, Re=5), "intermediate condition, mixed convection based on Nusselt numbers is appropriate")
  expect_identical(free_or_forced_convection(1,10), "forced convection")
  expect_identical(free_or_forced_convection(1000,1), "free convection")
})