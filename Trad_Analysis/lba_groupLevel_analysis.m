%% set paths
ddir = 'C:/Users/lasch/OneDrive/Documents/Wright State/Research/Cara_experiment_code/From Lab/Keep/';
addpath('C:/Users/lasch/OneDrive/Documents/Wright State/1-Fall2018/PSY9035-CognitiveModeling/LBA-master/LBA-master');

maxidnum = 54; % don't use 49, accuracy <70%


%% read in actual response times and accuracies & get in correct format
% odd ID = no aid (120 x 2), decide (120 x 4), reject (120 x 4)
% even ID = reject (120 x 4), no aid (120 x 2), decide (120 x 4)
% order 1 = LE, LD, HE, HD
% order 2 = LD, LE, HD, HE
% order 3 = HE, HD, LE, LD
% order 4 = HD, HE, LD, LE

real_data = ["ID" "Order" "Difficulty" "False Alarm" "Decision" "Signal"...
    "Automation Reponse" "Human Response" "Human RT" "Accuracy"];

for fi = 1:maxidnum
    if fi == 14 || fi == 24 || fi == 20 || fi == 49 || fi == 51 || fi == 53 % have overall accuracy < 70% (determined in R)
        continue
    end
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
end

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
    
% set RTs < 0.25 for 'reject' condition to zero
% for i = 1:length(real_data)
%     if real_data(i,5)=="REJECT" && str2double(real_data(i,9))<.25
%         real_data(i,9) = '0.0';
%     end
% end

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

%% bar plot of acc means and sds

for i = 1:10
    stats_acc(i,:) = [mean(str2double(real_data(real_data(:,12)==string(i),10)))...
        std(str2double(real_data(real_data(:,12)==string(i),10)))];
    stats_RT(i) = median(str2double(real_data(real_data(:,12)==string(i),9)));
end



figure
bar(stats_acc(:,1),'b')
hold on
errorbar(stats_acc(:,1),stats_acc(:,2), '.', 'Color','k', 'LineWidth',2)
ylabel('mean accuracy')
xlabel('test condition')

%% create the data structure. 
% need condition, rt, responses (human answer), stimulus code (signal, correct response) 

data.stim = str2double(real_data(2:end,11));  % correct response
data.cond = str2double(real_data(2:end,12));  % condition
data.rt = str2double(real_data(2:end,9)).*1000;  % RT in milliseconds
data.response = str2double(real_data(2:end,8));  % human response

hold off
min(data.rt);
figure(1)
histogram(data.rt, 'normalization','probability')
xlim([0,2500])

%% use LBA_clean to remove outliers
cutoffs = [100 4160];
data = LBA_clean(data, cutoffs);

hold on
histogram(data.rt, 'normalization','probability')
hold off

%% run LBA model fit

% create a model structure with the parameters either fixed (1) or varied/free.
% if free the lba calculates an optimal parameter for each condition. if
% fixed the lba calculates a single optimal parameter for all conditions.
% Each field must be a scalar equal to either 1 or Ncond.
% E.g. to share bounds between 3 conditions, but to keep drift rates
% constant, set:
% model.v = 1; model.A = 1; model.b = 3; model.sv = 1; model.t0 = 1;

% A = range of uniform distribution U[0,A] from which starting point k is
% drawn
% b = bound
% v = vector of drift rates
% sv = standard deviation of drift rate
% t0 = non-decision time
% N = number of response options

% Here I compare the fit of three models - model 1 fixes all paraneters 
% between conditions; model 2 allows drift rate to vary between conditions 
% and fixes all other parameters; model 3 allows bound to vary between 
% conditions and fixes all other parameters; model 4 allows drift rate and
% bond to vary between conditions and fixes all other parameters

nReps = 1;  % 200 took a very long time to run

for rep = 1:nReps
%     models = 1;
    % models = 2;
%     models = 3;
    models = 4;
    model(1).v = 1; model(1).A = 1; model(1).b = 1; model(1).t0 = 1; model(1).sv = 1;
    model(2).v = 10; model(2).A = 1; model(2).b = 1; model(2).t0 = 1; model(2).sv = 1;
    model(3).v = 1; model(3).A = 1; model(3).b = 10; model(3).t0 = 1; model(3).sv = 1;
    model(4).v = 10; model(4).A = 1; model(4).b = 10; model(4).t0 = 1; model(4).sv = 1; 
%     model(1).v = 10; model(1).A = 1; model(1).b = 10; model(1).t0 = 1; model(1).sv = 1; 
    v = 2*rand.*[.8 .7 .8 .7 .7 .6 .7 .6 .6 .5];
    b = 2*rand.*[150 150 150 200 200 200 300 300 300 300];
%     v = [.8 .7 .8 .7 .7 .6 .7 .6 .6 .5];
%     b = [150 150 200 200 200 200 300 300 300 300];

    % pArray = [v A b-A sv t0]; 
    pArray{1} = [rand*.6 rand*400 rand*250 rand*0.2 rand*1];
    pArray{2} = [v rand*400 rand*250 rand*0.2 rand*1];
    pArray{3} = [rand*.6 rand*400 b rand*0.2 rand*1];
%     pArray{1} = [.6 400 250 .2 1];
%     pArray{2} = [v 400 250 .2 1];
%     pArray{3} = [.6 400 b .2 1];
    pArray{4} = [v rand*400 b rand*0.2 rand*1];
%     pArray{1} = [v rand*400 b rand*0.2 rand*1];

    names{1} = ['v \t A \t b \t sv \t t0'];
    names{2} = ['v1 \t v2 \t v3 \t v4 \t v5 \t v6 \t v7 \t v8 \t v9 \t v10 \t A \t b \t sv \t t0'];
    names{3} = ['v \t A \t b1 \t b2 \t b3 \t b4 \t b5 \t b6 \t b7 \t b8 \t b9 \t b10 \t sv \t t0'];
    names{4} = ['v1 \t v2 \t v3 \t v4 \t v5 \t v6 \t v7 \t v8 \t v9 \t v10 \t A \t b1 \t b2 \t b3 \t b4 \t b5 \t b6 \t b7 \t b8 \t b9 \t b10 \t sv \t t0'];
%     names{1} = ['v1 \t v2 \t v3 \t v4 \t v5 \t v6 \t v7 \t v8 \t v9 \t v10 \t A \t b1 \t b2 \t b3 \t b4 \t b5 \t b6 \t b7 \t b8 \t b9 \t b10 \t sv \t t0'];

    for m = 1:models
        [params{rep,m} LL(rep,m)] = LBA_mle(data, model(m), pArray{m});
    end
end

for m = 1:models
    [val(m),idx(m)] = min(abs(LL(:,m)));
    opt_LL(m) = -val(m); 
    opt_params{m} = params{idx(m),m};
end

%% Compare plots of data and model fit
for m = 1:models
    
    % Fit models
    Ncond = max(data.cond);
    cor = data.response == data.stim;
    [v A b sv t0] = LBA_parse(model(m), opt_params{m}, Ncond);
 
    % Plot data and predictions
    LBA_plot(data, opt_params{m}, model(m));
    
    % Print out parameters
    fprintf('\n\n Model %d parameters: \n\n',m);
    fprintf(['\n' names{m} '\n\n']);
    fprintf(num2str(opt_params{m}));
    fprintf('\n\n Model %d log-likelihood: \n',m);
    fprintf(['\n' num2str(opt_LL(m)) '\n\n']);
end

%% chi-squared to compare models

x9 = chi2inv(0.95,9);  % critical chi-square value for alpha=.05 and 9 DOF is 16.9190
x18 = chi2inv(0.95,18);  % critical chi-square value for alpha=.05 and 18 DOF is 28.8693

% LL is the log likelihood so don't need to take the log again in
% the chi-squared calculation (eqn 10.2, pg 249)
% still need to include the - in front of the 2 though. 
% not entirely sure this is right, but the numbers don't seem unreasonable.

chi_sq_1v2 = -2*opt_LL(1) - (-2*opt_LL(2));  % model 2 is more general, model 1 is specific
chi_sq_1v3 = -2*opt_LL(1) - (-2*opt_LL(3));  % model 3 is more general, model 1 is specific

chi_sq_2v4 = -2*opt_LL(2) - (-2*opt_LL(4));  % model 4 is more general, model 2 is specific
chi_sq_3v4 = -2*opt_LL(3) - (-2*opt_LL(4));  % model 4 is more general, model 3 is specific


%% different code to plot data vs predicted - only model 1
% not used in paper
Ncond = max(data.cond);
cor = data.response == data.stim;
[v a b sv t0] = LBA_parse(model(1), params{1}, Ncond);

nbin = 10;
minrt = 100;
maxrt = max(data.rt);
bins = linspace(minrt, maxrt, nbin);
bindiff = bins(3)-bins(2);

figure;
set(gcf,'position',[150 500 450.*Ncond 250]);
for j = 1:Ncond
    subplot(1,Ncond,j);
    
    % corrects
    histdata = hist(data.rt(cor & data.cond == j), bins);
    vi = [v(j) 1-v(j)];
    histpred = LBA_n1PDF(bins-t0(j),a(j),b(j)+a(j),vi,sv(j));
    % histpred is likelihood of a single trial at this rt
    % need to scale to area of bar (bindiff*trials)
    histpred = histpred.*bindiff.*sum(data.cond == j);
    h = bar(bins, histdata);
    set(h, 'edgecolor','b','linewidth',1,'facecolor','w');
    hold on
    plot(bins, histpred, 'b','linewidth',2);
     
    % errors
    histdata = hist(data.rt(~cor & data.cond == j), bins);
    vi = [1-v(j) v(j)];
    histpred = LBA_n1PDF(bins-t0(j),a(j),b(j)+a(j),vi,sv(j));
    % histpred is likelihood of a single trial at this rt
    % need to scale to area of bar (bindiff*trials)
    histpred = histpred.*bindiff.*sum(data.cond == j);
    h = bar(bins+(bindiff/2), histdata);
    set(h, 'edgecolor','r','linewidth',1,'facecolor','w');
    hold on
    plot(bins+(bindiff/2), histpred, 'r','linewidth',2);
    xlabel('rt');
    ylabel('freq');
    
end

%% histograms of response time for each condition, correct and incorrect responses

% by condition only
figure;
set(gcf,'position',[150 500 450.*Ncond 250]);
for j = 1:Ncond
    % real correct data
    rt_correct = str2double(real_data(and(real_data(:,12)==string(j),real_data(:,10)=='1'),9))*1000; 
    rt_correct(isoutlier(rt_correct),:) = [];
    figure
    histogram(rt_correct,'FaceColor','b')
    xlim([0, 2500])
    ylim([0 600])
    
    % real incorrect data
    rt_wrong = str2double(real_data(and(real_data(:,12)==string(j),real_data(:,10)=='0'),9))*1000; 
    rt_wrong(isoutlier(rt_wrong),:) = [];
    hold on
    histogram(rt_wrong,'FaceColor','r')
    xlabel('response time (ms)')
    title(['condition ' + string(j)])
end

% by condition and participant
% figure;
% set(gcf,'position',[150 500 450.*Ncond 250]);
for j = 7%:8  % accept/reject conditions
% for j = 3:6  % direct decision conditions
    temp_data = real_data(real_data(:,12)==string(j),:);
%     figure
    % real correct data
    for k=1%:maxidnum %[1 16 23 41]
        rt_correct = str2double(temp_data(and(temp_data(:,10)=='1',temp_data(:,1)==string(k)),9))*1000; 
        rt_correct(isoutlier(rt_correct),:) = [];
        figure
        histogram(rt_correct,'FaceColor','b')
%         histogram(rt_correct,'BinWidth',40)
        hold on
        xlim([0, 2500])
        ylim([0 40])
%     xlabel('response time (ms)')
%     title(['condition' string(j) 'correct responses']    
        % real incorrect data
        rt_wrong = str2double(temp_data(and(temp_data(:,10)=='0',temp_data(:,1)==string(k)),9))*1000; 
        rt_wrong(isoutlier(rt_wrong),:) = [];
        hold on
        histogram(rt_wrong,'FaceColor','r')
        xlabel('response time (ms)')
        title(['condition ' + string(j) + ' / ID ' + string(k)])
%         xlim([0, 2500])
%         ylim([0 75])
    end
%     xlabel('response time (ms)')
%     title(['condition' string(j) 'incorrect responses'])
end

% by condition and response
figure;
set(gcf,'position',[150 500 450.*Ncond 250]);
for j = 8  % all conditions
    temp_data = real_data(real_data(:,12)==string(j),:);
    % real correct data
    for k=1:2
        rt_correct = str2double(temp_data(and(temp_data(:,10)=='1',temp_data(:,8)==string(k-1)),9))*1000; 
        rt_correct(isoutlier(rt_correct),:) = [];
        figure
        histogram(rt_correct,'FaceColor','b')
        xlim([0, 2500])
        ylim([0 500])
    
        % real incorrect data
        rt_wrong = str2double(temp_data(and(temp_data(:,10)=='0',temp_data(:,8)==string(k-1)),9))*1000; 
        rt_wrong(isoutlier(rt_wrong),:) = [];
        hold on
        histogram(rt_wrong,'FaceColor','r')
        xlabel('response time (ms)')
        title(['condition ' + string(j) + ' / response ' + string(k-1)])
    end
end

for i=1
    filename = ['figure'+string(i)+'.png'];
    A = figure(i);
    saveas(A,filename) 
end