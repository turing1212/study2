%% Load data
load('meats.mat');
X = zscore(meats(:,1:100));
Y = zscore(meats(:,101:103));
%max_components = size(X_train_inner,2);
max_components = 20;

%% Define the outer cross-validation settings
K = 10; % Number of folds
num_repeats = 50; % Number of times to repeat the inner loop
% rng(?)
outer_CV = cvpartition(size(Y,1),'KFold',K);

%% Preallocate arrays to store results
cellMSEinner = cell(K,1);
cellVARinner = cell(K,1);
opti_ncomp = zeros(K,1);
MSEouter = zeros(K,1);
BETAouter = cell(K,1);
Wouter = cell(K,1);
%% Outer cross-validation loop
for k = 1:K
    % Split data into training and test sets for this fold
    train_id = training(outer_CV,k);
    test_id = test(outer_CV,k);
    X_train = X(train_id,:);
    Y_train = Y(train_id,:);
    X_test = X(test_id,:);
    Y_test = Y(test_id,:);
    
    % Preallocate space to store results
    MSEinner = zeros(max_components,num_repeats);
    VARinner = zeros(max_components,num_repeats);

    %% Inner cross-validation loop
    for r = 1:num_repeats
        % Define the inner cross-validation settings
        % rng(?)
        inner_CV = cvpartition(size(Y_train,1),'HoldOut',0.3);
        
        % Split data into training and validation sets for this repeat
        train_idx = training(inner_CV,1);
        val_idx = test(inner_CV,1);
        X_train_inner = X_train(train_idx,:);
        Y_train_inner = Y_train(train_idx,:);
        X_val = X_train(val_idx,:);
        Y_val = Y_train(val_idx,:);
        
        % Perform partial least squares regression with different numbers of components
        %max_components = min(size(X_train_inner,2),size(X_train_inner,1)-1);
        num_components = 1:max_components;
        for c = 1:length(num_components)
            [~,~,~,~,beta,PCTVAR] = plsregress(X_train_inner,Y_train_inner,num_components(c));
            Y_val_pred = [ones(size(X_val,1),1) X_val]*beta;
            MSEinner(c,r) = mean(sum((Y_val-Y_val_pred).^2,2));
            VARinner(c,r) = sum(PCTVAR(2,:));
        end
                                                                                                                                                                                                                                                                                                                                                                                         
    end
    
    % store results for plot
    cellMSEinner{k,1} = MSEinner;
    cellVARinner{k,1} = VARinner;
    
    %% Re-fit model
    % tabulate(opti_ncomp_rep)
    % Select the optimal number of components based on validation error, method 1
%     MSEval = mean(MSEinner,2);
%     [~,opt_idx] = min(MSEval);
%     opti_ncomp(k) = num_components(opt_idx);
    
    % Select the optimal number of components based on validation error, method 2
    [~,opti_ncomp_rep] = min(MSEinner);
    opti_ncomp(k) = mode(opti_ncomp_rep);
    
    % Re-fit the model on the full training set with the optimal number of components
    [~,~,~,~,beta,~,~,stats] = plsregress(X_train,Y_train,opti_ncomp(k));
    
    % Compute the MSE for this fold
    Y_test_pred = [ones(size(X_test,1),1) X_test]*beta;
    MSEouter(k) = mean(sum((Y_test - Y_test_pred).^2,2));
    BETAouter{k,1} = beta;
    Wouter{k,1} = stats.W;
end

%% Estiblish the best model 
% Find the optimal ncomp in outer test
[~, best_idx] = min(MSEouter);
mean_MSEouter = mean(MSEouter);
opti_comp_outer = opti_ncomp(best_idx);

% reestiblish the best model
% X_train = X(outer_CV.training(best_idx),:);
% Y_train = Y(outer_CV.training(best_idx),:);
X_test = X(outer_CV.test(best_idx),:);
Y_test = Y(outer_CV.test(best_idx),:);
Y_pred = [ones(size(X_test,1),1) X_test]*BETAouter{best_idx,1};

% calculate VIP
statsW = Wouter{best_idx,1};
W0 = statsW ./ sqrt(sum(statsW,1));
p = size(XL,1);
sumSq = sum(XS.^2,1).*sum(YL.^2,1);
vipScore = sqrt(p* sum(sumSq.*(W0.^2),2) ./ sum(sumSq,2));
indVIP = find(vipScore >= 1);
scatter(1:length(vipScore),vipScore,'x')
hold on
scatter(indVIP,vipScore(indVIP),'rx')
plot([1 length(vipScore)],[1 1],'--k')
hold off
axis tight
xlabel('Predictor Variables')
ylabel('VIP Scores')