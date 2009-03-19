
function st_local_default = ixf_set_prop(st_local_default,totArg,varargin)
%--------------------------------------------------------------------------
%function syntax: st_local_default = ixf_set_prop(st_local_default,totArg,prop_name,prop_value,...)
%purpose:set properties of default structure
%input: st_local_default(structure), total Arguments, property name and
%values
%output: structure (st_local_default)
%example: st_local_default = ixf_set_prop(st_local_default,5,'grid','on')
%the above example sets the structure's grid property to on
%--------------------------------------------------------------------------



%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%parse
for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg

    switch(lower(varargin{iArgLoop}))
        %figure
    
        case 'color'
            color = varargin{iArgLoop+1};
            flag = ixf_validate_params('c',color);
            if ~flag
                ixf_display_error(IXG_ST_ERROR.not_valid_color,color)
            end
            st_local_default.plot.color = color;
        case 'facecolor'
            facecolor = varargin{iArgLoop+1};
            if (~isnumeric(facecolor))
                 ixf_display_error(IXG_ST_ERROR.invalid_number,'facecolor');
            end
            st_local_default.plot.facecolor;
        case 'facealpha'
            facealpha = varargin{iArgLoop+1};
            if ~ isnumeric(facealpha)
                  ixf_display_error(IXG_ST_ERROR.invalid_number,'facealpha');
            end
            st_local_default.plot.facealpha = facealpha;
        case 'transparency'
            transparency = varargin{iArgLoop+1};
            if ~ isnumeric(facealpha)
                  ixf_display_error(IXG_ST_ERROR.invalid_number,'transparency');
            end
            st_local_default.plot.facealpha = transparency;
            st_local_default.plot.edgealpha = transparency;
        case 'title'
            title=varargin{iArgLoop+1};
            if (~ischar(title) && ~iscell(title))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'title');
            end
            st_local_default.axes.title=title;
        case 'afontsize'
            afontsize=varargin{iArgLoop+1};
            if (~isnumeric(afontsize))
                 ixf_display_error(IXG_ST_ERROR.invalid_number,'afontsize')
            end
            st_local_default.axes.afontsize=afontsize;
        case 'acolor'
            afontcolor=varargin{iArgLoop+1};
            if (~ischar(afontcolor))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'acolor')
            end
            st_local_default.axes.acolor=afontcolor;                        
        case 'xlabel'
            xlabel=varargin{iArgLoop+1};
            if (~ischar(xlabel) && ~iscell(xlabel))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'xlabel')
            end
            st_local_default.axes.xlabel=xlabel;
        case 'ylabel'
            ylabel=varargin{iArgLoop+1};
            if (~ischar(ylabel) && ~iscell(ylabel))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'ylabel')
            end
            st_local_default.axes.ylabel=ylabel;
        case 'zlabel'
            zlabel=varargin{iArgLoop+1};
            if (~ischar(zlabel) && ~iscell(zlabel))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'zlabel')
            end
            st_local_default.axes.zlabel=zlabel;
        case 'xgrid'
            xgrid=varargin{iArgLoop+1};
            if (~ischar(xgrid))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'xgrid')
            end
            st_local_default.axes.xgrid=xgrid;
        case 'ygrid'
            ygrid=varargin{iArgLoop+1};
            if (~ischar(ygrid))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'ygrid')
            end
            st_local_default.axes.ygrid=ygrid;
        
        case 'zgrid'
             zgrid=varargin{iArgLoop+1};
            if (~ischar(zgrid))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'zgrid')
            end
            st_local_default.axes.zgrid=zgrid;
        
        case 'afontname'
             afontname=varargin{iArgLoop+1};
            if (~ischar(afontname))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'afontname')
            end
            st_local_default.axes.afontname=afontname;
        
        case 'afontunits'       % i.e. points 
            afontunits=varargin{iArgLoop+1};
            if (~ischar(afontunits))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'fontunits')
            end
            st_local_default.axes.fontunits=afontunits;
        
        case 'aposition'
        position = varargin{iArgLoop + 1};
        if (~isnumeric(position))
            ixf_display_error(IXG_ST_ERROR.invalid_number,'position');
        end
        st_local_default.axes.aposition = position;
        
        case 'afontcolor'
        fontcolor = varargin{iArgLoop + 1};
        if (~ischar(fontcolor))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'fontcolor');
        end
        st_local_default.axes.xcolor = fontcolor;
        st_local_default.axes.ycolor = fontcolor;
        st_local_default.axes.zcolor = fontcolor;
        case 'atag'
            atag=varargin{iArgLoop+1};
            if (~ischar(atag))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'atag')
            end
            st_local_default.axes.atag=atag;
       case 'ptag'
            ptag=varargin{iArgLoop+1};
            if (~ischar(ptag))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'ptag')
            end
            st_local_default.plot.ptag=ptag;
        case 'marker'
            marker=varargin{iArgLoop+1};
            if (~ischar(marker))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'marker')
            end
            st_local_default.plot.marker=marker;
        case 'markersize'
          markersize=varargin{iArgLoop+1};
            if (~isnumeric(markersize))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'markersize')
            end
            st_local_default.plot.markersize=markersize;
        case 'linestyle'
            linestyle=varargin{iArgLoop+1};
            if (~ischar(linestyle))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'linestyle')
            end
            st_local_default.plot.linestyle=linestyle;
        case 'xscale'           % log or lin 
            xscale=varargin{iArgLoop+1};
            if (~ischar(xscale))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'xscale')
            end
            st_local_default.axes.xscale=xscale;
        case 'yscale'
            yscale=varargin{iArgLoop+1};
            if (~ischar(yscale))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'yscale')
            end
            st_local_default.axes.yscale=yscale;
        case 'zscale'
            zscale=varargin{iArgLoop+1};
            if (~ischar(zscale))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'zscale')
            end
            st_local_default.axes.zscale=zscale;
        case 'xlim'
            xlim=varargin{iArgLoop+1};
            if (~isnumeric(xlim))
                 ixf_display_error(IXG_ST_ERROR.invalid_number,'xlim')
            end
            st_local_default.axes.xlim=xlim;
        case 'ylim'
            ylim=varargin{iArgLoop+1};
            if (~isnumeric(ylim))
                 ixf_display_error(IXG_ST_ERROR.invalid_number,'ylim')
            end
            st_local_default.axes.ylim=ylim;
        case 'zlim'
            zlim=varargin{iArgLoop+1};
            if (~isnumeric(zlim))
                 ixf_display_error(IXG_ST_ERROR.invalid_number,'zlim')
            end
            st_local_default.axes.zlim=zlim;
        case 'clim'
            clim=varargin{iArgLoop+1};
            if (~isnumeric(clim))
                 ixf_display_error(IXG_ST_ERROR.invalid_number,'clim')
            end
            st_local_default.axes.clim=clim;
            
        case 'fcolor'           % figure background color
            fcolor=varargin{iArgLoop+1};
            if(~ischar(fcolor))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fcolor')
            end
            st_local_default.figure.fcolor=fcolor;
       
        case 'funits'
        units = varargin{iArgLoop + 1};
        if (~ischar(units))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'units');
        end
        st_local_default.figure.funits = units;
        
        case 'tag'
            tag = varargin{iArgLoop + 1};
            if (~ischar(tag))
                    ixf_display_error(IXG_ST_ERROR.invalid_character,'tag');
            end
            st_local_default.figure.tag = tag;
        case 'name'
            name = varargin{iArgLoop + 1};
            if (~ischar(name))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'name');
            end
            st_local_default.figure.name = name;            
        case 'afontangle'
                fontangle  = varargin{iArgLoop + 1};
                if (~ischar(fontangle))
                    ixf_display_error(IXG_ST_ERROR.invalid_character,'fontangle');
                end
                st_local_default.axes.afontangle = fontangle;
        case 'afontweight'
            fontweight  = varargin{iArgLoop + 1};
            if (~ischar(fontweight))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontweight');
            end
            st_local_default.axes.afontweight = fontweight;
        case 'fposition'        % figure position
              position = varargin{iArgLoop + 1};
                if (~isnumermic(position))
                    ixf_display_error(IXG_ST_ERROR.invalid_number,'position');
                end
                st_local_default.figure.fposition = position;   
                
       case 'menubar'
            menubar = varargin{iArgLoop + 1};        
            if (~ischar(menubar))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'menubar');
            end
            st_local_default.figure.menubar = menubar;
         
        case 'yaxislocation'            % set orientation of y and x axis
                ylocation=varargin{iArgLoop+1};
                if(~ischar(ylocation))
                    ixf_display_error(IXG_ST_ERROR.invalid_character,'yaxislocation');
                end
                st_local_default.axes.yaxislocation=ylocation;
        case 'xaxislocation'
            xlocation=varargin{iArgLoop+1};
            if(~ischar(xlocation))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'xaxislocation')
            end
            st_local_default.axes.xaxislocation=xlocation;
            
        case 'projection'
            projection=varargin{iArgLoop+1};
            if(~ischar(projection))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'projction')
            end
            st_local_default.axes.projection=projection;            
            
        case 'linewidth'
            linewidth=varargin{iArgLoop+1};
            if (~isnumeric(linewidth))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'linewidth')
            end
            st_local_default.plot.linewidth=linewidth;
        case 'drawmode'
            drawmode=varargin{iArgLoop+1};
            if (~ischar(drawmode))
                ixf_display_error(IXT_ST_ERROR.invalid_character,'drawmode')
            end
            st_local_default.axes.drawmode=drawmode;
        case 'colororder'
            colororder=varargin{iArgLoop+1};
            if (~isnumeric(colororder))
                ixf_display_error(IXT_ST_ERROR.invalid_number,'colororder');
            end
            st_local_default.axes.colororder=colororder;
     case 'grid'
        grid = varargin{iArgLoop + 1};
        if (~ischar(grid))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'grid');
        end
        st_local_default.axes.ygrid = grid;
        st_local_default.axes.xgrid = grid;
        st_local_default.axes.zgrid = grid;
    case 'resize'
        resize = varargin{iArgLoop + 1};        
        if (~ischar(resize))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'resize');
        end       
        st_local_default.figure.resize = resize;
    case 'papersize'
        papersize = varargin{iArgLoop + 1};        
        if (~isnumeric(papersize))
            ixf_display_error(IXG_ST_ERROR.invalid_number,'papersize');
        end               
        st_local_default.figure.paper.papersize = papersize;
    case 'paperposition'
        paperposition = varargin{iArgLoop + 1};        
        if (~isnumeric(paperposition))
            ixf_display_error(IXG_ST_ERROR.invalid_number,'paperposition');
        end   
        st_local_default.figure.paper.paperposition = paperposition;
    case 'paperunits'
        paperunits = varargin{iArgLoop + 1};        
        if (~ischar(paperunits))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'paperunits');
        end              
        st_local_default.figure.paper.paperunits = paperunits;
    case 'paperorientation'
        paperorientation = varargin{iArgLoop + 1};        
        if (~ischar(paperorientation))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'paperorientation');
        end 
        st_local_default.figure.paper.paperorientation = paperorientation;
    case 'papertype'
        papertype = varargin{iArgLoop + 1};        
        if (~ischar(papertype))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'papertype');
        end               
        st_local_default.figure.paper.papertype = papertype;
    case 'paperpositionmode'
        paperpositionmode = varargin{iArgLoop + 1};        
        if (~ischar(papertype))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'paperpositionmode');
        end               
        st_local_default.figure.paper.paperpositionmode = paperpositionmode;
                %text
        case 'tcolor'
            col = varargin{iArgLoop + 1};
            st_local_default.text.tcolor = varargin{iArgLoop + 1}; 
        case 'tunits'
            units = varargin{iArgLoop + 1};
            if (~ischar(units))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'units');
            end
            st_local_default.text.tunits = varargin{iArgLoop + 1};                     
        case 'ttag'
            tag = varargin{iArgLoop + 1};
            if (~ischar(tag))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'tag');
            end
            st_local_default.text.ttag = varargin{iArgLoop + 1};                     
       
        case 'tcolor'
            col = varargin{iArgLoop + 1};
            st_local_default.text.tcolor = varargin{iArgLoop + 1}; 
            
        case 'tposition'
            position = varargin{iArgLoop + 1};
            if (~isnuermic(position))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'position');
            end
            st_local_default.text.tposition = varargin{iArgLoop + 1};                           
        case 'horizontalalignment'
            horizontalalignment  = varargin{iArgLoop + 1};
            if (~ischar(horizontalalignment))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'horizontalalignment');
            end
            st_local_default.text.horizontalalignment = varargin{iArgLoop + 1};                     
        case 'verticalalignment'
            verticalalignment  = varargin{iArgLoop + 1};
            if (~ischar(verticalalignment))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'verticalalignment');
            end
            st_local_default.text.verticalalignment = varargin{iArgLoop + 1};                     
        case 'tfontname'
            fontname  = varargin{iArgLoop + 1};
            if (~ischar(fontname))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontname');
            end
            st_local_default.text.tfontname = varargin{iArgLoop + 1};                     
        case 'tfontsize'
            fontsize  = varargin{iArgLoop + 1};
            if (~isnumeric(fontsize))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'fontsize');
            end
            st_local_default.text.tfontsize = varargin{iArgLoop + 1};                     
        case 'tfontangle'
            fontangle  = varargin{iArgLoop + 1};
            if (~ischar(fontangle))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontangle');
            end
            st_local_default.text.tfontangle = varargin{iArgLoop + 1};                     
        case 'tfontweight'
            fontweight  = varargin{iArgLoop + 1};
            if (~ischar(fontweight))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontweight');
            end
            st_local_default.text.tfontweight = varargin{iArgLoop + 1};                     
        case 'tfontunits'
            fontunits  = varargin{iArgLoop + 1};
            if (~ischar(fontunits))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontunits');
            end
            st_local_default.text.tfontunits = varargin{iArgLoop + 1};
        case 'closerequestfcn'
            close_req=varargin{iArgLoop+1};
            if (~ischar(close_req))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'close_request');
            end
            st_local_default.figure.CloseRequestFcn=close_req;
        case 'na'
            % do nothing - but don't display error
        case 'type'
            type=varargin{iArgLoop+1};
            if (~ischar(type))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'type');
            end
            st_local_default.plot.type=type;
        case 'shading'
            shading_val=varargin{iArgLoop+1};
            if (~ischar(shading_val))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'Shading');
            end
            st_local_default.plot.shading=shading_val;
        case 'separation'
            separation=varargin{iArgLoop+1};
            if (~ischar(separation)&& ~isempty(separation))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'separation');
            end
            st_local_default.plot.separation=separation;
        case 'levels'
            levels=varargin{iArgLoop+1};
            if (~isnumeric(levels))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'Levels');
            end
            st_local_default.plot.levels=levels;
        case 'noxvalues'
        case 'noyvalues'            
        case 'default'
        case 'format'
            format = varargin{iArgLoop+1};
            if ~ischar(format)
                ixf_display_error(IXG_ST_ERROR.invalid_character,'format')
            end
            st_local_default.plot.format = format;
            
        case 'counter'
            counter = varargin{iArgLoop+1};
            if ~isnumeric(counter)
                ixf_display_error(IXG_ST_ERROR.invalid_number,'counter')
            end
            st_local_default.figure.counter = counter;
            
        case 'colormap'
            color = varargin{iArgLoop +1};
            if ~ ischar(color)
                ixf_display_error(IXG_ST_ERROR.invalid_character,'colormap')
            end
            st_local_default.plot.colormap = color;
        
        case 'dataaspectratio'
            st_local_default.axes.dataaspectratio = varargin{iArgLoop +1};
        
        case 'aspect'
            st_local_default.axes.aspect = varargin{iArgLoop +1};
        
        case 'color_slider'
            st_local_default.axes.color_slider = varargin{iArgLoop +1};
            
        case 'facecolor'
            st_local_default.plot.facecolor = varargin{iArgLoop +1};
            
        case 'x_sliderlabel'
            st_local_default.axes.x_sliderlabel = varargin{iArgLoop +1};
 
        case 'y_sliderlabel'
            st_local_default.axes.y_sliderlabel = varargin{iArgLoop +1};
            
        case 'z_sliderlabel'
            st_local_default.axes.z_sliderlabel = varargin{iArgLoop +1};
        case 'aunits'
            st_local_default.axes.aunits = varargin{iArgLoop +1};    
        case 'isonormals'
            if ~islogical( varargin{iArgLoop +1})
                ixf_display_error(IXG_ST_ERROR.invalid_number,'isonormals')
            end
            st_local_default.figure.isonormals = varargin{iArgLoop +1};    

        
        otherwise
          ixf_display_error(IXG_ST_ERROR.wrong_field, varargin{iArgLoop});
           
    end
end