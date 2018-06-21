
/*=============================================================================
cue_comp.cpp
===============================================================================
Main file of a model exploring social competence. Individuals play the 
traditional hawl-dove game. The porpose of the project is to add a type of 
player that uses a morphological cue to determine the probability to play
hawk or dove. 


Written by :

Andrés E.Quiñones
Posdoctoral researcher
Behavioural Ecology Group
Institute of Biology
University of Neuchâtel
Switzerland

Start date :
11 June 2018
=============================================================================*/


#include <iostream>
#include <fstream>
#include "M:\\Routines\\C++\\RandomNumbers\\random.h"
#include <cstdlib>
#include <math.h>
#include "D:\\quinonesa\\Dropbox\C++\\json.hpp"       
// Header for reading and using JSON files see https://github.com/nlohmann/json

using namespace std;

// Classes

enum strategy { hawk, dove, evaluator};

// counters for statistics

int countGenotypes[3] = { 0,0,0 };
int countPhenotypes[2] = { 0,0 };

class individual {
public:
	individual(strategy genotype_, double own_cue_);
	individual(individual& mother, double mutRate);
	double curr_payoff;
	double cum_payoff;
	int ninterac;
	strategy phenotype;
	strategy get_strat() {
		return(genotype);
	}
	double get_cue() {
		return(own_cue);
	}
	void set_phenotype(individual partner);
	void get_payoff(individual partner, double param[]);
	double logist(double othercue);
	strategy mutate(strategy genotype,double mutRate);
private:
	strategy genotype;
	double own_cue;
};

// constructors
individual::individual(strategy genotype_, double own_cue_) {
	genotype = genotype_;
	own_cue  = own_cue_ ;
	curr_payoff = 0;
	cum_payoff = 0;
	ninterac = 0;
}

individual::individual(individual& mother, double mutRate) {
	genotype = mutate(mother.genotype,mutRate);
	own_cue = mother.own_cue;
	curr_payoff = 0;
	cum_payoff = 0;
	ninterac = 0;
}


void individual::get_payoff(individual partner,double payoff_matrix[]) {      
	/*  (This are IDs to the entries of the matrix, NOT payoffs)    
	        Hawk      Dove     
	   Hawk   0         1
	   Dove   2         3
	*/
	int scenario = phenotype + 2 * partner.phenotype;
	curr_payoff =  payoff_matrix[scenario];
	cum_payoff  += payoff_matrix[scenario];
	++ninterac;
}

double individual::logist(double othercue) { 
	return (1 / (1 + exp(-(get_cue() - othercue)))); 
}

void individual::set_phenotype(individual partner) {
	if (get_strat() == evaluator)
	{
		double probHawk = logist(partner.get_cue());
		if (rnd::uniform() < probHawk) {
			phenotype = hawk;
		}
		else {
			phenotype = dove;
		}
	}
	else {
		phenotype = get_strat();
	}
	++countPhenotypes[phenotype];
}

std::string itos(int j) {				// turns int into string
	std::stringstream s;
	s << j;
	return s.str();
}

std::string douts(double j) {			// turns double into string
	std::stringstream s;
	s << j;
	return s.str();
}

string create_filename(std::string filename, double *payoff_matrix, 
	int seed) {
	// name the file with the parameter specifications
	filename.append("_HH");
	filename.append(douts(payoff_matrix[0]));
	filename.append("_HD");
	filename.append(douts(payoff_matrix[1]));
	filename.append("_DH");
	filename.append(douts(payoff_matrix[2]));
	filename.append("_DD");
	filename.append(douts(payoff_matrix[3]));
	filename.append("_seed");
	filename.append(itos(seed));
	filename.append(".txt");
	return(filename);
}

void Reprod(vector<individual> popT, const int popsize) {
	vector<individual> popTplus1(popsize);
	rnd::discrete_distribution payoff_dist(popsize);
	for (vector<individual>::iterator itpop = popT.begin(), int i = 0; 
		itpop < popT.end(); ++itpop,++i) {
		payoff_dist[i] = itpop->cum_payoff/itpop->ninterac;
	}	
	for (vector<individual>::iterator itpopTplus1 = popTplus1.begin();
		itpopTplus1 < popTplus1.end(); ++itpopTplus1) {
		*itpopTplus1 = individual(popT[payoff_dist.sample()]);
	}
	countGenotypes[0] = 0, countGenotypes[1] = 0, countGenotypes[3] = 0;
	vector<individual>::iterator itpopTplus1 = popTplus1.begin();
	for (vector<individual>::iterator itpopT = popT.begin();
		itpopT < popT.end(); ++itpopT, ++itpopTplus1) {
		*itpopT = *itpopTplus1;
		++countGenotypes[*itpopT->get_strat];
	}
}

void interactions(vector<individual> population,int nint, int popsize,
	double payoff_matrix[4]) {
	int ind1 = popsize;
	int ind2 = popsize;
	countPhenotypes[0] = 0;
	countPhenotypes[2] = 0;
	for (int i = 0; i < nint*popsize; ++i) {
		while (ind1 == ind2) {
			ind1 = rnd::integer(popsize);
			ind2 = rnd::integer(popsize);
		}
		population[ind1].set_phenotype(population[ind2]);
		population[ind2].set_phenotype(population[ind1]);
		population[ind1].get_payoff(population[ind2], payoff_matrix);
		population[ind2].get_payoff(population[ind1], payoff_matrix);
		ind1 = popsize, ind2 = popsize;
	}
}

void printStats(int popsize,ofstream &output, int time) {
	int totInt = countPhenotypes[0] + countPhenotypes[1];
	output << countGenotypes[hawk]      / popsize << '\t';
	output << countGenotypes[dove]      / popsize << '\t';
	output << countGenotypes[evaluator] / popsize << '\t';
	output << countPhenotypes[hawk]     / totInt << '\t';
	output << countPhenotypes[dove]     / totInt << '\t';

}

void initializeIndFile(ofstream &indOutput, double payoff_matrix[4],
	nlohmann::json param) {
	std::string namedir = param["folder"];
	// 
	std::string folder;
	namedir.append(folder);
	string IndFile = create_filename(namedir, &payoff_matrix[4],param["seed"]);
	indOutput.open(IndFile.c_str());
	indOutput << "time" << '\t' << "freqGenHawks" << '\t' << "freqGenDove";
	indOutput << '\t' << "freqGenEval" << '\t' << "freqFenHawks" << '\t';
	indOutput << "freqFenDoves" << '\t' << "meanCue" << '\t' << "sdCue";
	indOutput << endl;
}