-- anu-astro1x

-- flux and luminosity

lum2flux l r = l / (4 * pi * r * r)

flux2lum f r = f * 4 * pi * r * r

flux2dist l f = sqrt (l / (4 * pi * f))

ratio2dist rat r = r * (sqrt rat)

-- distances and sizes

parsec = 3.086e16

megaparsec = 1e6 * parsec

kiloparsec = 1000 * parsec

au = 1.5e11

rad deg = (deg / 180) * pi

deg rad = (180 * rad) / pi

arcsec = arcmin / 60

arcmin = 1 / 60

size2angle dist r = r / dist

rad2arcsec r = (deg r) / arcsec

arcsec2rad as = rad (as * arcsec)

theta2dist th r = r / th

theta2radius th dist = th * dist

m2au m = m / au

-- redshift

c = 3.0e8

redshift lam lam0 = (lam - lam0) / lam0

red2vel red = red * c

vel2red v = v / c

-- density and energy evolution

ev = 1.6e-19

et at = (1e-3 * ev) / at

rho0 = 5e-28

rho z = rho0 * (1 + z) ^ 3

-- scale factors

z at = (1 - at) / at

at z = 1 / (1 + z)

me = 9.1e-31 -- mass of electron

-- quasars

flux' l d z = l / (4 * pi * d * d * (1 + z) * (1 + z))

evap m c dt = m * c * dt -- energy to vaporize

u = 1.6605e-27 -- atomic mass

e2m m = m * c * c

rs m = (2 * g * m) / (c * c)

g = 6.673e-11

gpe m1 m2 r = (-1 * g * m1 * m2) / r

hu = 1.00794 * u -- hydrogen mass

heu = 4.002602 *u -- helium mass

cau = 12.010 * u

lsun = 3.8e26 -- luminosity of sun in watts

rearth = 6400 * 1000.0

mearth = 6.0e24 -- kilograms

ke m v = 0.5 * m * v * v

-- n-body codes

accel m r = (g * m) / (r * r)

-- gamma ray bursts

beam v = sqrt $ 1 - (v / c) ^ 2

deltaflux theta = 4.0 / (theta * theta)

-- Hertzsprung-Russell

mass v r = (v * v * r) / g

-- matthew bate, university of exeter

angular_momentum m v r = m * v * r -- this is conserved

centripetal_force m v r = (m * v * v) / r

-- life in space

boltzman = 5.67e-8

-- boltzman equation: L = A * boltzman * T^4
-- A is surface area, L = luminosity, T = temperature in kelvin

eq_temp l d = (l / (16 * pi * boltzman * d * d)) ** 0.25

eq_dist t l = sqrt $ l / (16 * pi * boltzman * t * t * t * t)

-- anu-astro2x

-- pulsar planets

reflex_period_to_r ms p = ((g * ms * p * p) / (4 * pi * pi)) ** (1/3)

-- velocity of star due to reflex motion

reflex_period_to_v mp ms p r = (2 * pi * r * mp) / (p * ms)

-- note true velocity = v * (sin i), where i is angle of inclination

reflex_period_to_mp v p ms = (v * p * ms) / (2 * pi * r)
                   where r = reflex_period_to_r ms p

-- radius of star motion proportional to m * r
reflex_star_radius mp ms rp = (rp * mp) / ms

-- again radius is times sin(i)

reflex_star_planet_mass ms rp rs = (rs * ms) / rp

-- reflex motion

-- delta in brightness from transit
transit_to_radius b db rs = rs * sqrt (db / b)

-- inclination determination
-- transit time for perfect edge on
edge_on_transit_time r v = (2 * r) / v

vel_to_period r v = (2 * pi * r) / v

period_to_vel r p = (2 * pi * r) / p

-- gravitational lensing

lens_theta m r = (4 * g * m) / (r * c * c)

einstein_radius m d = sqrt $ (4 * g * m) / (d * c * c)

-- debris disks

-- wien's displacement law, l is wavelength at which the emission peaks
-- t is the temperature
wien_lambda t = 2.9e-3 / t

wien_temp l = 2.9e-3 / l

-- fraction of a stars light intercepted by planet of radius r at distance d

intercepted_fraction r d = (r * r) / (4 * d * d)

-- adaptive optics

-- l is lambda, d is diameter of lens
diffraction_limit l d = l / d 

-- r is radius of planet, d is distance to star
brightness_ratio r d = 0.25 * (r / d) * (r / d)

-- direct imaging

lum_and_temp_to_r l t = sqrt $ l / (boltzman * t * t * t * t * 4 * pi)

est_grav_energy m r = (g * m * m) / (4 * r)

-- earth-like planets

boltzman_constant = 1.38e-23

gas_vel t m = sqrt $ (3 * boltzman_constant * t) / m

mass_h = 1.66e-27

mass_o = 5.31e-26

kinetic_energy m v = 0.5 * m * v * v

escape_vel m r = sqrt $ (2 * g * m) / r

planck_constant = 6.62e-34

energy_per_photon l = planck_constant * mu
    where mu = c / l

-- astro-3x violent universe

x3hq2_1 r2 = (4 * pi * pi * r1 * (r1 + r2) * (r1 + r2)) / (g * p * p)
  where  p = 100.0 * 365 * 86400
         r1 = 1e12 

x3hq2_2 r2 = m1 * r1 / r2
  where m1 = 3e30
        r1 = 1e12

x3hq2_3 = map x3hq2_1 [1e12 + x*1e11 | x <- [0..29]]

x3hq2_4 = map x3hq2_2 [1e12 + x*1e11 | x <- [0..29]]

-- [1e12 + x*1e11 | x <- [0..29]] !! 21, feed that into either equation

lum r t = a * boltzman * t ** 4
   where a = 4 * pi * r * r

x3hq3_1 = ra * (ta / tb) * (ta / tb) * sqrt (1 / 500)
   where ra = 700000 * 1000.0
         ta = 6000
         tb = 10000

-- classical novae

thomson_cross_section = 6.7e-29

mass_proton = 1.67e-27

eddington_limit_lum m = (4 * pi * g * m * mass_proton * c) / thomson_cross_section

-- astro3x hq 4.2

eddington_limit_mass l = (thomson_cross_section * l) / (4 * pi * g * mass_proton * c)

energy_to_flux e s = e / s

x3hq4_2 = eddington_limit_mass lum
   where lum  = energy_to_flux e week
         week = 7 * 86400
         e    = 1e44

-- thermonuclear supernovae, violent universe chapter 5

explosion_velocity absorption_lambda emission_lambda = c * (delta_lambda / emission_lambda)
  where delta_lambda = absorption_lambda - emission_lambda
