%% Time-Frequency Analysis and Cluster-Based Permutation Tests example

%% clean environment
clear; close all; clc;
eeglab; close;
ft_defaults;

%% set directory
dir_r = pwd;
dir_d = [dir_r, '\epoched']; 
dir_tf = [dir_r, '\time_frequency_data']; 
dir_stats = [dir_r, '\stats_results'];  

%get file name
cd(dir_d)
folderInfo = dir;
folderInfo = folderInfo(~ismember({folderInfo.name}, {'.', '..'}));
folderli = {folderInfo.name};
folderlist = folderli(endsWith(folderli, '.set'));
num_participants = length(folderlist);

% Analysis parameters from the manuscript
FREQ_RANGE = [2 40]; % Frequency range of interest 
N_CYCLES = [3 0.8]; 
NTIMESOUT = 400;  

for i = 1:num_participants
    % Load preprocessed EEG data
    EEG = pop_loadset('filename', filelist{i}, 'filepath', dir_d);
    
    % Calculate ERSP using Morlet wavelets
    [ersp, itc, powbase, times, freqs] = pop_newtimef(EEG, 1, 1:EEG.nbchan, [EEG.xmin EEG.xmax]*1000, N_CYCLES, ...
        'baseline', NaN, 'freqs', FREQ_RANGE, 'plotersp', 'off', ...
        'plotitc', 'off', 'ntimesout', NTIMESOUT, 'padratio', 1);
    
    % Save the resulting time-frequency data
    output_filename = fullfile(dir_tf, [erase(filelist{i}, '.set'), '_tf.mat']);
    save(output_filename, 'ersp', 'times', 'freqs', 'EEG');
end

%% Prepare for FieldTrip

cd(dir_d);
filelist = dir('*_BF_*.set'); % Example to get only BF condition files
participant_list = unique(extractBefore({filelist.name}, '_'));
n_subj = length(participant_list);

bf_mw_data = cell(1, n_subj);
bf_focus_data = cell(1, n_subj);
template_eeg = []; 

for i = 1:n_subj
    mw_filename = fullfile(dir_tf, [participant_list{i}, '_BF_MW_state_tf.mat']);
    if exist(mw_filename, 'file')
        loaded_data = load(mw_filename);
        bf_mw_data{i} = loaded_data.ersp;
        if isempty(template_eeg)
            template_eeg = loaded_data.EEG; % Store info for chanlocs
        end
    else
        warning('MW file not found for subject %s', participant_list{i});
    end
    
    % Load Focus state data for the current subject
    focus_filename = fullfile(dir_tf, [participant_list{i}, '_BF_Focus_state_tf.mat']);
    if exist(focus_filename, 'file')
        loaded_data = load(focus_filename);
        bf_focus_data{i} = loaded_data.ersp;
        if isempty(template_eeg) % Also get template if MW was missing
            template_eeg = loaded_data.EEG;
            freqs = loaded_data.freqs; % Need freqs for conversion
        end
    else
        warning('Focus file not found for subject %s', participant_list{i});
    end
end

bf_mw_avg = cellfun(@(x) squeeze(mean(x, 3)), bf_mw_data, 'UniformOutput', false);
bf_focus_avg = cellfun(@(x) squeeze(mean(x, 3)), bf_focus_data, 'UniformOutput', false);

[ft_bf_mw, ft_bf_focus] = deal(cell(1, n_subj));

for i = 1:n_subj
    ft_bf_mw{i} = create_ft_struct(bf_mw_avg{i}, freqs, template_eeg.chanlocs);
    ft_bf_focus{i} = create_ft_struct(bf_focus_avg{i}, freqs, template_eeg.chanlocs);
end

cfg = [];
cfg.method           = 'montecarlo';
cfg.statistic        = 'ft_statfun_depsamplesT'; 
cfg.correctm         = 'cluster';
cfg.clusteralpha     = 0.05;  
cfg.clusterstatistic = 'maxsum'; 
cfg.minnbchan        = 2;     
cfg.tail             = 0;   
cfg.clustertail      = 0;
cfg.alpha            = 0.025; 
cfg.numrandomization = 1000; 

cfg.design = [1:n_subj 1:n_subj; ones(1, n_subj) 2*ones(1, n_subj)];
cfg.uvar   = 1; % Subject variable
cfg.ivar   = 2; % Independent variable (condition)

% Define channel neighbors
cfg_neighb = [];
cfg_neighb.method    = 'distance';
cfg_neighb.elec      = ft_bf_mw{1}.elec; % Use elec info from one subject's data
cfg.neighbours       = ft_prepare_neighbours(cfg_neighb);

[stat_bf_focus_vs_mw] = ft_freqstatistics(cfg, ft_bf_focus{:}, ft_bf_mw{:});

% Save the result
output_stat_file = fullfile(dir_stats, 'stat_bf_focus_vs_mw.mat');
save(output_stat_file, 'stat_bf_focus_vs_mw');

%% --- Helper function ---
function ft_data = create_ft_struct(powermap, freqs, chanlocs)
    % This is a simplified converter function. A real one might need more info.
    ft_data = [];
    ft_data.label = {chanlocs.labels};
    ft_data.freq = freqs;
    ft_data.dimord = 'chan_freq';
    ft_data.powspctrm = powermap;
    % Convert EEGLAB chanlocs to FieldTrip elec structure
    ft_data.elec = ft_read_sens_eeglab(struct('chanlocs', chanlocs, 'unit', 'mm'));
end