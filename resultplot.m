%% Draw line graph of MSE
MSEmat = zeros(max_components,num_repeats);
for k = 1:K
    MSEmat = (MSEmat + cellMSEinner{k,1})/10;
end
MSEvec = mean(MSEmat,2);
[minMSE,minid] = min(MSEvec);
plot(1:max_components,MSEvec, 'b-');
hold on
plot(minid,minMSE, 'ro');
xlabel('Number of components');
ylabel('MSE');
title('MSE versus Number of Components');
%% Draw line graph of the percent of variance explained 
VARmat = zeros(max_components,num_repeats);
for k = 1:K
    VARmat = (VARmat + cellVARinner{k,1})/10;
end
VARvec = mean(VARmat,2);
corVAR = VARvec(minid);
figure(2)
plot(1:max_components,VARvec, 'g-');
hold on
plot(minid,corVAR, 'ro');
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


