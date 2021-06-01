# -*- coding: utf-8 -*-
"""
Created on Wed Nov 11 11:57:32 2020

@author: Marco Zini

Create a dictionary which cointain all the custom settings for the
data visualization


"""

from cycler import cycler

class paper:
    axis_width = 1.5
    tick_label_size = 35
    minor_ticks_count = 2
    subplot_spacing = 0.35
    
    grid_major_alpha = 0.7
    
    dict = {
        #### Font ####
        'font.family'    : 'sans-serif',
        'font.sans-serif': 'Arial',        
        # Title
        'axes.titlesize': 23,
        'axes.titlepad' : 15,        
        # Axes Label
        'axes.labelsize': 35,
        'axes.labelpad' : 15,        
        # Tick Label
        'xtick.labelsize': tick_label_size,
        'xtick.major.pad': 5,
        'ytick.labelsize': tick_label_size,
        'ytick.major.pad': 5,
        
        # Legend
        'legend.fontsize'      : 28,
        'legend.title_fontsize': 26,
        
        #### Figure ####
        'figure.figsize': (10, 8),
        'figure.dpi'    : 300,
        
        #### Sub Plot Spacing ####
        'figure.subplot.hspace': subplot_spacing,
        'figure.subplot.wspace': subplot_spacing,
        
        #### Axes ####
        'axes.facecolor' : 'w',
        'axes.edgecolor' : 'k',
        'axes.linewidth' : axis_width,
        'axes.prop_cycle': cycler(color=['#0a04bf', '#bf0000', '#242424', '#00ab03']),
        
        
        #### Ticks ####
        # x ticks
        'xtick.bottom'       : True,
        'xtick.major.size'   : 12,
        'xtick.major.width'  : axis_width,
        'xtick.minor.visible': True,
        'xtick.minor.size'   : 6,
        'xtick.minor.width'  : axis_width,
        # y ticks
        'ytick.left'         : True,
        'ytick.major.size'   : 12,
        'ytick.major.width'  : axis_width,
        'ytick.minor.visible': True,
        'ytick.minor.size'   : 6,
        'ytick.minor.width'  : axis_width,
        
        #### Grid ####
        'axes.grid'      : True,
        'axes.grid.which': 'major',    
        'grid.linestyle' : ':',
        'grid.linewidth' : 1.5,
        'grid.color'     : '#adadad',
        'grid.alpha'     : grid_major_alpha,
        
        #### Legend ####
        'legend.framealpha' : 1,
        'legend.edgecolor'  : 'k',
        'legend.markerscale': 4,
        'legend.fancybox'   : False,
        
        #### Plots ####
        # Lines
        'lines.linewidth': 2.5,
        
        # Histogram
        'hist.bins' : 50,
        # Scatter
        'scatter.marker'       : 'o',
        'scatter.edgecolors'   : 'none',
        'lines.markeredgewidth': 0,
        'lines.markersize'     : 5,
        'lines.markeredgecolor': 'k',
        
        #### Date and Time ####
        'date.autoformatter.year' : '%Y',
        'date.autoformatter.month': '%m',
        'date.autoformatter.day'  : '%d',
        'date.autoformatter.hour' : '%H',
        
        #### Save Figures ####
        'savefig.dpi'   : 300,
        'savefig.format': 'png',
        'savefig.bbox'  : 'tight',
    }
    
    text_box_props = {'alpha'    : 1,
                      'edgecolor': 'k',
                      'facecolor': 'w',
                      'fill'     : True,
                      'linewidth': axis_width}
    
    figure_letter = {'alpha'    : 1,
                     'edgecolor': 'none',
                     'facecolor': 'none',
                     'fill'     : False,
                     'linewidth': 0}
        

class slideshow:
    axis_width = 1.5
    tick_label_size = 25
    minor_ticks_count = 2
    subplot_spacing = 0.35
    
    grid_major_alpha = 0.7
    
    dict = {
        #### Font ####
        'font.family'    : 'sans-serif',
        'font.sans-serif': 'Arial',        
        # Title
        'axes.titlesize': 27,
        'axes.titlepad' : 15,        
        # Axes Label
        'axes.labelsize': 25,
        'axes.labelpad' : 15,        
        # Tick Label
        'xtick.labelsize': tick_label_size,
        'xtick.major.pad': 5,
        'ytick.labelsize': tick_label_size,
        'ytick.major.pad': 5,
        
        # Legend
        'legend.fontsize'      : 20,
        'legend.title_fontsize': 26,
        
        #### Figure ####
        'figure.figsize': (10, 8),
        'figure.dpi'    : 50,
        
        #### Sub Plot Spacing ####
        'figure.subplot.hspace': subplot_spacing,
        'figure.subplot.wspace': subplot_spacing,
        
        #### Axes ####
        'axes.facecolor' : 'w',
        'axes.edgecolor' : 'k',
        'axes.linewidth' : axis_width,
        'axes.prop_cycle': cycler(color=['#0a04bf', '#ff0000', '#242424', '#00ab03']),
        
        
        #### Ticks ####
        # x ticks
        'xtick.bottom'       : True,
        'xtick.major.size'   : 12,
        'xtick.major.width'  : axis_width,
        'xtick.minor.visible': True,
        'xtick.minor.size'   : 6,
        'xtick.minor.width'  : axis_width,
        # y ticks
        'ytick.left'         : True,
        'ytick.major.size'   : 12,
        'ytick.major.width'  : axis_width,
        'ytick.minor.visible': True,
        'ytick.minor.size'   : 6,
        'ytick.minor.width'  : axis_width,
        
        #### Grid ####
        'axes.grid'      : True,
        'axes.grid.which': 'major',    
        'grid.linestyle' : ':',
        'grid.linewidth' : 1.5,
        'grid.color'     : '#adadad',
        'grid.alpha'     : grid_major_alpha,
        
        #### Legend ####
        'legend.framealpha' : 1,
        'legend.edgecolor'  : 'k',
        'legend.markerscale': 4,
        'legend.fancybox'   : False,
        
        #### Plots ####
        # Lines
        'lines.linewidth': 2.5,
        
        # Histogram
        'hist.bins' : 50,
        # Scatter
        'scatter.marker'       : 'o',
        'scatter.edgecolors'   : 'none',
        'lines.markeredgewidth': 0,
        'lines.markersize'     : 5,
        'lines.markeredgecolor': 'k',
        
        #### Date and Time ####
        'date.autoformatter.year' : '%Y',
        'date.autoformatter.month': '%m',
        'date.autoformatter.day'  : '%d',
        'date.autoformatter.hour' : '%H',
        
        #### Save Figures ####
        'savefig.dpi'   : 300,
        'savefig.format': 'png',
        'savefig.bbox'  : 'tight',
    }
    
    text_box_props = {'alpha'    : 1,
                      'edgecolor': 'k',
                      'facecolor': 'w',
                      'fill'     : True,
                      'linewidth': axis_width}
    
    figure_letter = {'alpha'    : 1,
                     'edgecolor': 'none',
                     'facecolor': 'none',
                     'fill'     : False,
                     'linewidth': 0}