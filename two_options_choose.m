function choice = two_options_choose(pchoice)

    choice = nan(size(pchoice));

    for t = 1:length(pchoice)
        if rand(1) < pchoice(t) %bigger the pchoice, more likely to choose "1"
            choice(t) = 1;
        else
            choice(t) = 0;
        end
    end
    
end
