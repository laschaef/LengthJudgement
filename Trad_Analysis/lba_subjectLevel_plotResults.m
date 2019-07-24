%% set paths
ddir = 'C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/';
addpath('C:/Users/lasch/OneDrive/Documents/Wright State/1-Fall2018/PSY9035-CognitiveModeling/LBA-master/LBA-master');

maxidnum = 54; 

%%  load LBA model fit results
load([ddir '/ind_LL_150reps.mat'])
load([ddir '/ind_mod_LL_150reps.mat'])
load([ddir '/ind_opt_LL_150reps.mat'])
load([ddir '/ind_opt_params_150reps.mat'])
load([ddir '/ind_optParamsSummary_150reps.mat'])
load([ddir '/ind_params_150reps.mat'])


%% 

tic

% odd ID = no aid (120 x 2), decide (120 x 4), reject (120 x 4)
% even ID = reject (120 x 4), no aid (120 x 2), decide (120 x 4)
% order 1 = LE, LD, HE, HD
% order 2 = LD, LE, HD, HE
% order 3 = HE, HD, LE, LD
% order 4 = HD, HE, LD, LE

for fi = 1:maxidnum
% for fi = [5, 7, 11, 15, 17, 21, 23, 27, 33, 39, 41, 46, 47, 54]
% for fi = [1, 2, 6, 8, 9, 10, 12, 13, 16, 18, 19, 22, 26, 28, 29, 30, 32, 34, 35, 36, 37, 38, 40, 42, 43, 44, 45, 48, 50, 52]
    if fi == 14 || fi == 24 || fi == 20 || fi == 49 || fi == 51 || fi == 53 % have overall accuracy < 70% (determined in R)
        continue
    end
    
    clear real_data
    real_data = ["ID" "Order" "Difficulty" "False Alarm" "Decision" "Signal"...
    "Automation Reponse" "Human Response" "Human RT" "Accuracy"];
    
    % read in actual response times and accuracies & get in correct format
    fid = fopen([ ddir 'Mahoney_exp_' num2str(fi) '.txt'],'r');
    % this code splits each line by tabs
    while ~feof(fid)  % while not the end of the identified file
        aline = fgetl(fid);  % getl ends in 'l' for line
        aline = regexp(aline,'\t','split');
    
        % add check to look for 'id' in the line.
        if any(strcmpi('ID',aline))
            continue % skip to the next iteration of the loop
        end
             
        real_data(end+1,:) = [cellfun(@string,aline(1)) cellfun(@string,aline(3)),...
            cellfun(@string,aline(5)), cellfun(@string,aline(6)), cellfun(@string,aline(8)),...
            cellfun(@string,aline(10)), cellfun(@string,aline(13)) cellfun(@string,aline(11))...
            cellfun(@string,aline(12)), cellfun(@string,aline(15))];
        
        order(fi) = cellfun(@str2double,aline(3));
    end
    fclose(fid);
    
    % add column with stimulus (signal for no aid and decide, compare human
    % response to signal for reject)
    real_data(1,end+1) = "Correct Response";

    for i = 2:length(real_data)
        if real_data(i,5) == 'NO AID' || real_data(i,5) == 'DECIDE'
            real_data(i,end) = real_data(i,6);
        else
            if real_data(i,6) == real_data(i,7) 
                real_data(i,end) = 0;
            elseif real_data(i,6) ~= real_data(i,7)
                real_data(i,end) = 1;
            end
        end
    end
        
    % set RTs >= 4.16  to zero
    for i = 1:length(real_data)
        if str2double(real_data(i,9))>=4.16
            real_data(i,9) = '0.0';
        end
    end
    
    % remove zero RTs
    real_data(real_data(:,9)=='0.0',:) = [];
    
    % add column with conditions
    real_data(1,end+1) = "Condition";

    for i = 1:length(real_data)
        if real_data(i,3) == 'easy' && real_data(i,4)=='n/a' && real_data(i,5) == 'NO AID'
            real_data(i,end) = 1;  % decide_nae
        elseif real_data(i,3) == 'hard' && real_data(i,4)=='n/a' && real_data(i,5) == 'NO AID'
            real_data(i,end) = 2;   % decide_nad
        elseif real_data(i,3) == 'easy' && real_data(i,4)=='low' && real_data(i,5) == 'DECIDE'
            real_data(i,end) = 3;   % decide_LE
        elseif real_data(i,3) == 'hard' && real_data(i,4)=='low' && real_data(i,5) == 'DECIDE'
            real_data(i,end) = 4;   % decide_LD
        elseif real_data(i,3) == 'easy' && real_data(i,4)=='high' && real_data(i,5) == 'DECIDE'
            real_data(i,end) = 5;   % decide_HE
        elseif real_data(i,3) == 'hard' && real_data(i,4)=='high' && real_data(i,5) == 'DECIDE'
            real_data(i,end) = 6;   % decide_HD
        elseif real_data(i,3) == 'easy' && real_data(i,4)=='low' && real_data(i,5) == 'REJECT'
            real_data(i,end) = 7;   % reject_LE
        elseif real_data(i,3) == 'hard' && real_data(i,4)=='low' && real_data(i,5) == 'REJECT'
            real_data(i,end) = 8;   % reject_LD
        elseif real_data(i,3) == 'easy' && real_data(i,4)=='high' && real_data(i,5) == 'REJECT'
            real_data(i,end) = 9;   % reject_HE
        elseif real_data(i,3) == 'hard' && real_data(i,4)=='high' && real_data(i,5) == 'REJECT'
            real_data(i,end) = 10; % reject_HD
        end 
    end
    
    % add a column ("Previous") that has true/false (1/0) of whether previous trial was incorrect
    % Previous: if previous trial incorrect = 1, previous trial correct = 0 (same as R code)
    real_data(1,end+1) = "Previous";
    real_data(2,end) = 0;  % first trial, so no previous trial
    for i = 3:length(real_data)
        if str2double(real_data(i-1,10)) == 0
            real_data(i,end) = 1;
        else
            real_data(i,end) = 0;
        end
    end
    
    % create the data structure. 
    % need condition, rt, responses (human answer), stimulus code (signal, correct response) 
    data.stim = str2double(real_data(2:end,11));  % correct response
    data.cond = str2double(real_data(2:end,12));  % condition
    data.rt = str2double(real_data(2:end,9)).*1000;  % RT in milliseconds
    data.response = str2double(real_data(2:end,8));  % human response

    % use LBA_clean to remove outliers
%     cutoffs = [100 4160];
    cutoffs = [100 2000];
    data = LBA_clean(data, cutoffs);

    % define model(s)  
    models = 4;  
    model(1).v = 1; model(1).A = 1; model(1).b = 1; model(1).t0 = 1; model(1).sv = 1;
    model(2).v = 10; model(2).A = 1; model(2).b = 1; model(2).t0 = 1; model(2).sv = 1;
    model(3).v = 1; model(3).A = 1; model(3).b = 10; model(3).t0 = 1; model(3).sv = 1;
    model(4).v = 10; model(4).A = 1; model(4).b = 10; model(4).t0 = 1; model(4).sv = 1; 

    % get participant's model fit parameters
    params = ind_params{fi};
    LL = ind_LL{fi};

    modLL = ind_mod_LL{fi};
    opt_LL = ind_opt_LL{fi}; 
    opt_params = ind_opt_params{fi};
    
%     for m = 1:models
%         [val(m),idx(m)] = min(abs(LL(:,m)));
%         opt_LL(m) = -val(m); 
%         opt_params{m} = params{idx(m),m};
%     end
    
    % Compare plots of data and model fit
    for m = 1:models
    
        % Fit models
        Ncond = max(data.cond);
        cor = data.response == data.stim;
        [v A b sv t0] = LBA_parse(model(m), opt_params{m}, Ncond);

        % Plot data and predictions
        LBA_plot(data, opt_params{m}, model(m));
        title(['Subject ' + string(fi)] + ' Model ' + string(m))
        


        % Print out parameters
%         fprintf('\n\n Model %d parameters: \n\n',m);
%         fprintf(['\n' names{m} '\n\n']);
%         fprintf(num2str(opt_params{m}));
%         fprintf('\n\n Model %d log-likelihood: \n',m);
%         fprintf(['\n' num2str(opt_LL(m)) '\n\n']);
    end
    
    for i=1:4
        filename = ['subject'+string(fi)+'model'+string(i)+'.png'];
        A = figure(i);
        saveas(A,filename) 
    end
    close all
    
    % Compare plots of data and model fit - only for model 4, for each participant
    % Fit models
%     Ncond = max(data.cond);
%     cor = data.response == data.stim;
%     [v A b sv t0] = LBA_parse(model(4), opt_params_summary(fi,:), Ncond);
% 
%     % Plot data and predictions
%     LBA_plot(data, opt_params_summary(fi,:), model(4));
%     title(['Subject ' + string(fi)])
            
    % Print out parameters
%     fprintf('\n\n Model %d parameters: \n\n',m);
%     fprintf(['\n' names{m} '\n\n']);
%     fprintf(num2str(opt_params{m}));
%     fprintf('\n\n Model %d log-likelihood: \n',m);
%     fprintf(['\n' num2str(opt_LL(m)) '\n\n']);     
end



toc


