%% EEG Preprocessing Script

%set directory
dir_r = pwd; 
dir_d =  [dir_r,'\data_mff']; %data pass
dir_e =  [dir_r,'\all_events']; %event files pass
dir_proc = [dir_r,'\preprocessed_EEG']; %output directory

cd(dir_d)
folderInfo = dir;
folderInfo = folderInfo(~ismember({folderInfo.name}, {'.', '..'}));
folderlist = {folderInfo([folderInfo.isdir]).name};

N = length(folderlist);

cd(dir_e)
folderev = dir;
folderev = folderev(~ismember({folderev.name}, {'.', '..'}));
eventlist = {folderev.name};

 for i = 1:N
    % --- Load Data and Events ---
    cd(dir_d)
    EEG = pop_mffimport(folderlist{i},{'code'},0,0);

    cd(dir_e)
    EEG = pop_importevent( EEG, 'event',eventlist{i},'fields',{'type','latency'},'skipline',1,'timeunit',1,'align',0);
     
    % --- Basic Processing ---
    EEG = pop_resample(EEG, 250);
    EEG = pop_select( EEG, 'channel',{'E1','E2','E3','E4','E5','E6','E7','E8','E9','E10','E11','E12','E13','E14','E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26','E27','E28','E29','E30','E31','E32','E33','E34','E35','E36','E37','E38','E39','E40','E41','E42','E43','E44','E45','E46','E47','E48','E49','E50','E51','E52','E53','E54','E55','E56','E57','E58','E59','E60','E61','E62','E63','E64','E65'});
    EEG = pop_eegfiltnew(EEG, 'locutoff',0.5,'hicutoff',50,'plotfreqz',0);
    
    % --- Channel Rejection and Interpolation ---
    originalEEG = EEG;
    EEG = pop_clean_rawdata(EEG, 'FlatlineCriterion','off','ChannelCriterion',0.85,'LineNoiseCriterion',4,'Highpass','off','BurstCriterion','off','WindowCriterion','off','BurstRejection','off','Distance','Euclidian');
    EEG = pop_interp(EEG, originalEEG.chanlocs, 'spherical');

    % --- Line Noise Removal (Cleanline) ---
    signal      = struct('data', EEG.data, 'srate', EEG.srate);
    lineNoiseIn = struct('lineNoiseMethod', 'clean', ...
                         'lineNoiseChannels', 1:EEG.nbchan,...
                         'Fs', EEG.srate, ...
                         'lineFrequencies', [50 100 150 200 250],...
                         'p', 0.01, ...
                         'fScanBandWidth', 2, ...
                         'taperBandWidth', 2, ...
                         'taperWindowSize', 4, ...
                         'taperWindowStep', 1, ...
                         'tau', 100, ...
                         'pad', 2, ...
                         'fPassBand', [0 EEG.srate/2], ...
                         'maximumIterations', 10);
     [clnOutput, lineNoiseOut] = cleanLineNoise(signal, lineNoiseIn);
     EEG.data = clnOutput.data;

    % --- Data Cleaning (Burst Correction) ---
    EEG = pop_clean_rawdata(EEG, 'FlatlineCriterion','off','ChannelCriterion','off','LineNoiseCriterion','off','Highpass','off','BurstCriterion',20,'WindowCriterion','off','BurstRejection','off','Distance','Euclidian');

    %rereference
    EEG = pop_reref( EEG, []);

    %ICA rejection
    pca_rank = rank(EEG.data);
    EEG = pop_runica(EEG, 'icatype', 'runica', 'extended', 1, 'interrupt', 'on', 'pca', pca_rank);
    
    % --- IC Classification and Rejection ---
    all_comps = 1:size(EEG.icaweights, 1);
    eeglab redraw;
    EEG = iclabel(EEG, 'default');
    EEG = pop_icflag(EEG, [NaN NaN; 0.8 1; 0.8 1; 0.8 1; 0.8 1; 0.8 1; 0.8 1]); % Flag components with >= 80% artifact probability
    rejected_comps = all_comps(EEG.reject.gcompreject);
    EEG = pop_subcomp(EEG, rejected_comps, 0);

    % --- Save Preprocessed Data ---
	cd(dir_proc);
	EEG = pop_saveset(EEG,'filename',folderlist{i});
end
