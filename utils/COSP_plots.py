#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

# (c) British Crown Copyright, the Met Office.
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted 
# provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright notice, this list 
#       of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright notice, this list
#       of conditions and the following disclaimer in the documentation and/or other materials 
#       provided with the distribution.
#     * Neither the name of the Met Office nor the names of its contributors may be used 
#       to endorse or promote products derived from this software without specific prior written 
#       permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR 
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
# FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER 
# IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
# OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import matplotlib.cm as mpl_cm
import matplotlib.pyplot as plt
import iris
import iris.quickplot as qplt
import numpy as np
import iris.analysis.cartography as icart

dbze94 = 'equivalent_reflectivity_factor'
atb532 = 'volume_attenuated_backwards_scattering_function_in_air'
cfadDbze94 = 'histogram_of_equivalent_reflectivity_factor_over_height_above_reference_ellipsoid'
cfadLidarsr532 = 'histogram_of_backscattering_ratio_over_height_above_reference_ellipsoid'
clisccp = 'cloud_area_fraction_in_atmosphere_layer'


######### area_average ########################################################
def area_average(x,latitude='latitude',longitude='longitude'):

    if x.coord(latitude).bounds is None:
        x.coord(latitude).guess_bounds()
    if x.coord(longitude).bounds is None:
        x.coord(longitude).guess_bounds()
    grid_areas = icart.area_weights(x)
    x_avg = x.collapsed([longitude, latitude], iris.analysis.MEAN, 
                            weights=grid_areas)
    return x_avg

######### change_data_units ##################################################
def change_data_units(x,std_name,new_units='%',factor=100):
    """
    Function that changes the units of a cube.
    """

    x = x * factor
    x.standard_name = std_name
    x.units = new_units
    return x

######### change_coord_units #################################################
def change_coord_units(x,coord_name='height',new_units='km',factor=0.001):
    """
    Function that changes the units of a coordinate.
    """

    coord = x.coord(coord_name)
    coord.points = coord.points * factor
    coord.units = new_units
    if coord.bounds is not None:
        coord.bounds = coord.bounds * factor

############### change_pseudo_level ###########################################
def change_pseudo_level(x, origin='pp'):
    """
    Function that changes the pp pseudo_level dimension to the correct units.
    """
    pname = 'pseudo_level'
    # Calculate values
    if x.standard_name == cfadDbze94:
        deltas = np.arange(15)*5
        points = -47.5 + deltas
        bounds = np.array( [ (-50.0 + deltas), (-45.0 + deltas) ] )
        bounds = bounds.transpose()
        units  = 'dBZ'
        std_name = 'equivalent_reflectivity_factor'
    elif x.standard_name == cfadLidarsr532:
        #points = np.array([0.005, 0.605, 2.1, 4, 6, 8.5, 12.5, 17.5, 
                           #22.5, 27.5, 35, 45, 55, 70, 50040])
        #vb = np.array([0, 0.01, 1.2, 3, 5, 7, 10, 15, 20, 
                      #25, 30, 40, 50, 60, 80, 100000])
        #bounds =   np.empty( (15,2), dtype=float)
        #bounds[:,0] = vb[0:15]
        #bounds[:,1] = vb[1:16]
        deltas = np.arange(15)
        points = 0.5 + deltas
        bounds = np.array( [ (0 + deltas), (1 + deltas) ] )
        bounds = bounds.transpose()
        units = '1'
        std_name = 'backscattering_ratio'
        if origin == 'nc':
            pname = std_name
    elif x.standard_name == clisccp:
        deltas = np.arange(7)
        points = 0.5 + deltas
        bounds = np.array( [ (0 + deltas), (1 + deltas) ] )
        bounds = bounds.transpose()
        units = '1'
        std_name = 'atmosphere_optical_thickness_due_to_cloud'
        if origin == 'nc':
            pname = std_name
            points = points[1:]
            bounds = bounds[1:][:]
    else:
        raise NameError('Pseudo level not recognised: ' + x.standard_name)

    # Assign values
    pseudo = x.coord(pname)
    pseudo.points = points
    pseudo.units = units
    pseudo.standard_name = std_name
    pseudo.bounds = bounds

######### cosp_plot_column_1D ##########################################################
def cosp_plot_column_1D(fnc, varname='equivalent_reflectivity_factor', column = 0, time = 0):
    """
    Function that plots one column of curtain data.
    """

    plt.interactive(True)
    fig=plt.figure()
    # Read cube
    z=iris.load(fnc)
    z=z[0]

    # Get coords
    c = z.coord('column')
    t = z.coord('time')

    # Select time and column
    y=z.extract(iris.Constraint(column=c.points[column]))
    y=y.extract(iris.Constraint(time=t.points[time]))

    qplt.pcolormesh(y)

    return

######### cosp_plot_column_2D ##########################################################
def cosp_plot_column_2D(fnc, varname='equivalent_reflectivity_factor', level=0, column = 0, time = 0):
    """
    Function that plots one column of lat/lon data.
    """

    plt.interactive(True)
    fig=plt.figure()
    ax = fig.add_subplot(111)
    # Read cube
    z=iris.load(fnc)
    z=z[0]

    # Get coords
    c = z.coord('column')
    t = z.coord('time')

    # Select time and column
    y=z.extract(iris.Constraint(column=c.points[column]))
    y=y.extract(iris.Constraint(time=t.points[time]))
    # Select level. Not managed to make constrain with 'atmospheric model level'
    y=y[level]

    color_map = mpl_cm.get_cmap('Paired')
    qplt.pcolormesh(y,cmap=color_map,vmin=-20,vmax=20)
    plt.gca().coastlines()

    return


######### cosp_plot_cfad_2D ##########################################################
def cosp_plot_cfad_2D(fnc, varname=cfadDbze94, time = 0):
    """
    Function that plots an area-averaged CFAD from a 2D file.
    """

    # Read cube
    z=iris.load(fnc)
    z=z[0]

    # Select time
    t = z.coord('time')
    z=z.extract(iris.Constraint(time=t.points[time]))


    # Obtain area average
    cfad = area_average(z)

    if varname is not clisccp:
        cfad = change_data_units(cfad,varname)
        change_coord_units(cfad,coord_name='altitude')

    if cfad.standard_name in [cfadLidarsr532, clisccp]:
        change_pseudo_level(cfad, origin = 'nc')

    # Set limits
    yticks = None
    if cfad.standard_name == cfadDbze94:
        xlim = [-30, 20]
        ylim = [  0, 18]
        vmax = 8
    elif cfad.standard_name == cfadLidarsr532:
        xlim = [ 3, 15]
        ylim = [ 0, 18]
        vmax = 3
        xt1 = xlim[0]
        xt2 = xlim[1] + 1
        xticks  = xt1 + np.arange(xt2 - xt1)
        xlabels = ['0', '0.01', '1.2', '3', '5', '7', '10', '15', '20', 
                  '25', '30', '40', '50', '60', '80', '']
    elif cfad.standard_name == clisccp:
        xlim = [ 0, 7]
        ylim = [ 1000, 100]
        vmax = 8
        xt1 = xlim[0]
        xt2 = xlim[1] + 1
        xticks  = xt1 + np.arange(xt2 - xt1)
        xlabels = ['0', '0.3', '1.3', '3.6', '9.4', '23', '60', '']
        yticks = [1000, 800, 680, 560, 440, 310, 180]
    else:
        raise NameError('Variable not supported: ' + x.standard_name)

    # Plot block plot
    plt.interactive(True)
    fig=plt.figure()
    ax = fig.add_subplot(111)
    qplt.pcolormesh(cfad,vmin=0,vmax=vmax)
    # Title and limits
    ax.set_xlim(xlim)
    ax.set_ylim(ylim)
    # Xlabels
    if cfad.standard_name == cfadLidarsr532:
        ax.set_xticks(xticks)
        ax.set_xticklabels(xlabels[xt1:xt2])
    if cfad.standard_name == clisccp:
        ax.set_xticklabels(xlabels)
    # Ylabels
    if yticks is not None:
        ax.set_yticks(yticks)

    return