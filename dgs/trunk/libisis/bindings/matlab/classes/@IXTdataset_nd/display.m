function display(dataset_nd)
% DISPLAY Display an ISISEXCsource object

disp(' ');
disp([inputname(1),' = '])
disp(' ');
disp(['## entry_name            =  ' dataset_nd.entry_name])
disp(['## name                  =  ' dataset_nd.name])
disp(['## title                 =  ' dataset_nd.title])
disp(['## nx                    =  ' num2str(dataset_nd.nx)]);
disp(['## s                     =  []']);
disp(['## e                     =  []']);
disp(['## s_label               =  ' dataset_nd.s_label])
disp(['## x                     =  ' num2str(dataset_nd.x)]); 
disp(['## x_label               =  ' dataset_nd.x_label])
disp(['## x_units               =  ' dataset_nd.x_units])
disp(['## x_distribution        =  []']);
disp(['## x_histogram           =  []']);

disp(' ');

if(dataset_nd.x_histogram==logical(0)),
    
    disp('to do ... HEY ! It is a DEMO OK?!');

end

if(dataset_nd.x_histogram==logical(1)),
    
    disp('to do ... HEY ! It is a DEMO OK?!');
    
end


