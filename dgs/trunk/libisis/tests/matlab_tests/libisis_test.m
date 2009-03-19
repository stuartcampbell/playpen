function varargout = libisis_test(varargin)
% optional inputs: 'full', 'lite', 'continuous', 'extra only', 'default structure' (provide
% a default structure as the next argument), 'detailed' (enters debug
% mode), publish - will publish the test and what it's doing.
% test script for GTK graphical package - run to test all functions perform
% as they should, documentation contained within the script itself, run
% script in debug mode to test each part individually, skip the erroneous
% data
%
% optional input: gtk_test('lite','continuous'), will run all tests with rebinned data,
% and may skip some tests so that the program will run on slower computers.
% It will not pause for user input at any moment
%% Obtain Variables
global lite_tag;
global full_tag;
global extra_tag;
global default_structure_tag;
global stop_tag; 
global publish_tag; 
global continuous_flag;

lite_tag = false;
full_tag = false;
extra_tag = false;
default_structure_tag = false;
stop_tag = false;
publish_tag = false;
continuous_flag = false;
libisis_flag = true;
gtk_flag = true;

display('all variables in the workspace will be cleared.')
display('A backup is stored in the current directory under ''backup.mat'', this')
display('will be retrieved once the test finishes. If the test fails, use')
display('>> load(''backup.mat'')')
display('to retrieve them')

evalin('base','save(''backup.mat'')');
evalin('base','clear variables');

if nargin > 0
    for i = 1:length(varargin)
        switch varargin{i}
            case 'lite'
                lite_tag = true;
            case 'full'
                full_tag = true;
            case 'extra only'
                extra_tag = true;
            case 'default structure'
                default_structure = varargin(i+1);
                default_structure_tag = true;
            case 'detailed'
                stop_tag = true;
            case 'publish'
                publish_tag = true;
            case 'continuous'
                continuous_flag = true;
            case 'libisis only'
                gtk_flag = false;
            case 'gtk only'
                libisis_flag = false;
            otherwise 
                warning('argument passed wasn''t recognised')
                display(varargin{i})
        end
    end
end

if stop_tag == true
    dbstop in gtk_test_script
    dbstop in gtk_test_script at 150
    dbstop in gtk_test_script at 300
    dbstop in gtk_test_script at 450
    dbstop in gtk_test_script at 600
    dbstop in gtk_test_script at 750
    dbstop in gtk_test_script at 900
    dbstop in gtk_test_script at 1000
end

try
    
    if publish_tag
        varargout{1} = publish('libisis_test_script');
        varargout{2} = publish('gtk_test_script');
    else 
        profile on
        if libisis_flag
            libisis_test_script
        end
        if gtk_flag
            gtk_test_script
        end
        profile off
    end

    profile viewer
catch
    clear all
    evalin('base','load(''backup.mat'')');
    close all
    errorinfo = lasterror;
        display('----------------------------------')
        display(['ERROR: ' errorinfo.message])
    for i = 1:size(errorinfo.stack)

        display(['file name: ' errorinfo.stack(i).file])
        display(['line: ' num2str(errorinfo.stack(i).line)])
    end
    
end

clear all
evalin('base','load(''backup.mat'')');