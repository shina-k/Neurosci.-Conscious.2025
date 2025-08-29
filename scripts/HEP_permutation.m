%% HEP Analysis using Cluster-Based Permutation Test example
% setup
clear; close all; clc;
eeglab; close;
ft_defaults;

dir_r = pwd;
dir_d = [dir_r, '\epoched_HEP_data']; 
dir_stats = [dir_r, '\stats_results'];
if ~isfolder(dir_stats), mkdir(dir_stats); end

ANALYSIS_LATENCY = [0 0.6];
cond_A.name = 'Focus_BF'; 
cond_A.file_pattern = '*_BF_Focus_state.set';
ond_B.name = 'MW_BF';    
cond_B.file_pattern = '*_BF_MW_state.set';

% load data
cd(dir_d);
filelist = dir(cond_A.file_pattern);
participant_list = extractBefore({filelist.name}, '_');
n_subj = length(participant_list);

data_A = cell(1, n_subj);
data_B = cell(1, n_subj);
valid_subj_mask = false(1, n_subj);

for i = 1:n_subj
    filename_A = fullfile(dir_d, [participant_list{i}, '_BF_Focus_state.set']);
    EEG_A = pop_loadset('filename', filename_A);
    % Data should already be averaged. If not, average it.
    if EEG_A.trials > 1, EEG_A.data = mean(EEG_A.data, 3); EEG_A.trials = 1; end
    data_A{i} = eeglab2fieldtrip(EEG_A, 'timelockanalysis', 'none');

    filename_B = fullfile(dir_d, [participant_list{i}, '_BF_MW_state.set']);
    if exist(filename_B, 'file')
        EEG_B = pop_loadset('filename', filename_B);
        if EEG_B.trials > 1, EEG_B.data = mean(EEG_B.data, 3); EEG_B.trials = 1; end
        data_B{i} = eeglab2fieldtrip(EEG_B, 'timelockanalysis', 'none');
        valid_subj_mask(i) = true; % Mark subject as valid
    else
        fprintf('Warning: Data for MW_BF condition not found for %s. Excluding subject.\n', participant_list{i});
    end
end

data_A = data_A(valid_subj_mask);
data_B = data_B(valid_subj_mask);
n_valid_subj = sum(valid_subj_mask);

cfg = [];
cfg.channel          = 'all'
cfg.latency          =  ANALYSIS_LATENCY;
cfg.method           = 'montecarlo';
cfg.statistic        = 'ft_statfun_depsamplesT';
cfg.correctm         = 'cluster';
cfg.clusteralpha     = 0.05;
cfg.clusterstatistic = 'maxsum';
cfg.minnbchan        = 2;
cfg.tail             = 0;
cfg.alpha            = 0.025;
cfg.numrandomization = 1000;

%set channel 
cfg_neighb = [];
cfg_neighb.method    = 'distance';
cfg_neighb.elec      = data_A{1}.elec;
cfg.neighbours       = ft_prepare_neighbours(cfg_neighb);
    
design = [1:n_valid_subj, 1:n_valid_subj; ones(1, n_valid_subj), 2*ones(1, n_valid_subj)];
cfg.design = design;
cfg.uvar   = 1; % Unit variable (subject)
cfg.ivar   = 2; % Independent variable (condition)
    
[stat] = ft_timelockstatistics(cfg, data_A{:}, data_B{:});

output_stat_file = fullfile(dir_stats, 'stat_HEP_Focus-vs-MW_BF.mat');
save(output_stat_file, 'stat');

