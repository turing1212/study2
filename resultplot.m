%% Draw line graph of MSE
MSEmat = zeros(max_components,num_repeats);
for k = 1:K
    MSEmat = MSEmat + cellMSEinner{k,1};
end
MSEmat = MSEmat / 10;
MSEvec = mean(MSEmat,2);
plot(1:max_components,MSEvec, 'b-');
hold on
plot(opti_ncomp(best_idx),MSEvec(opti_ncomp(best_idx)), 'ro');
xlabel('Number of components');
ylabel('MSE');
title('MSE versus Number of Components');
%% Draw line graph of the percent of variance explained 
VARmat = zeros(max_components,num_repeats);
for k = 1:K
    VARmat = VARmat + cellVARinner{k,1};
end
VARmat = VARmat / 10;
VARvec = mean(VARmat,2);
corVAR = VARvec(opti_ncomp(best_idx));
figure(2)
plot(1:max_components,VARvec, 'g-');
hold on
plot(opti_ncomp(best_idx),corVAR, 'ro');
xlabel('Number of components');
ylabel('The percent of variance explained ');
title('Percent of variance explained versus Number of Components');
%% Draw scatter plot of Ypredicted and Y (use model with best performance)
figure(3)
subplot(1,3,1)
scatter(Y_test(:,1), Y_pred(:,1),[],'r','filled')
hline = refline([1 0]);
xlabel('Predicted water');
ylabel('Water');

subplot(1,3,2)
scatter(Y_test(:,2), Y_pred(:,2),[],'b','filled')
hline = refline([1 0]);
xlabel('Predicted fat');
ylabel('Fat');

subplot(1,3,3)
scatter(Y_test(:,3), Y_pred(:,3),[],'g','filled')
hline = refline([1 0]);
xlabel('Predicted protein');
ylabel('Protein');
%% Draw VIP bar

%% Draw


