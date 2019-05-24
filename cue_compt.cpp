
/*=============================================================================
cue_comp.cpp
===============================================================================
Main file of a model exploring social competence. Individuals play the 
traditional hawk-dove game. The purpose of the project is to add a type of 
player that uses a morphological cue to determine the probability to play
hawk or dove. 


Written by :

Andr�s E.Qui�ones
Posdoctoral researcher
Behavioural Ecology Group
Institute of Biology
University of Neuch�tel
Switzerland

Start date :
11 June 2018
=============================================================================*/


#include <stdio.h>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <math.h>	
#include <vector>
#include "tchar.h"
#include "C:\\Users\\a.quinones\\Dropbox\\C++\\Routines\\C++\\RandomNumbers\\random.h"
#include "C:\\Users\\a.quinones\\Dropbox\C++\\json.hpp"       
// Header for reading and using JSON files see https://github.com/nlohmann/json

using namespace std;
using json = nlohmann::json;

// Classes

enum strategy { hawk, dove, evaluator};

// counters for statistics

int countGenotypes[3];
int countPhenotypes[2];
double BadgeMeanSd[2];
double alphaMeanSd[2];
double betaMeanSd[2];

class individual {
	public:
		individual(strategy genotype_,double alphaBadge_,double betaBadge_);
		individual(individual& mother, double mutRate,double mutSD);
		double curr_payoff;
		double cum_payoff;
		int ninterac;
		strategy phenotype;
		strategy get_strat() {
			return(genotype);
		}
		double get_badge() {
			return(own_badge);
		}
		double get_alpha() {
			return(alphaBadge);
		}
		double get_beta() {
			return(betaBadge);
		}
		void set_phenotype(individual partner);
		void get_payoff(individual partner, vector<double> param, bool win);
		void setBadge();
		double logist(double otherBadge);
		strategy mutateStr(strategy genotype,double mutRate);
		double mutateDoub(double value, double mutRate, double mutSD);
	private:
		strategy genotype;
		double own_badge;
		double quality;
		double alphaBadge;
		double betaBadge;
};

// set quality and badge of status
void individual::setBadge() {
	quality = rnd::normal(0.5);
	clip_range(quality, 0, 1);
	own_badge = 1 / (1 + exp(alphaBadge - betaBadge * quality));
}

// constructors
individual::individual(strategy genotype_=hawk, double alphaBadge_=0,
	double betaBadge_=0) {
	alphaBadge = alphaBadge_;
	betaBadge = betaBadge_;
	genotype = genotype_;
	setBadge();
	curr_payoff = 0;
	cum_payoff = 0;
	ninterac = 0;
}

individual::individual(individual& mother, double mutRate,double mutSD) {
	genotype = mutateStr(mother.genotype,mutRate);
	//own_cue = mother.own_cue;
	alphaBadge = mutateDoub(mother.alphaBadge,mutRate,mutSD);
	betaBadge = mutateDoub(mother.betaBadge,mutRate,mutSD);
	setBadge(); 
	curr_payoff = 0;
	cum_payoff = 0;
	ninterac = 0;
}

strategy individual::mutateStr(strategy genotype,double mutRate) {
	if (rnd::uniform() < mutRate) {
		strategy newgenotype = (strategy)rnd::integer(3);
		// set intput to random generator to 3, to include evaluators
		// set intput to random generator to 2, for simple hawk-dove game
		return(newgenotype);
	}
	else {
		return(genotype);
	}
}

double  individual::mutateDoub(double value, double mutRate, double mutSD) {
	if (rnd::uniform() < mutRate) {
		double newValue = value + rnd::normal(0,mutSD);
		return(newValue);
	}
	else {
		return(value);
	}
}


void individual::get_payoff(individual partner,vector<double> payoff_matrix, 
	bool win) {      
	/*  (This are IDs to the entries of the matrix, NOT payoffs)    
	        Hawk      Dove     
	   Hawk   0         1
	   Dove   2         3
	*/
	int scenario = 2 * phenotype + partner.phenotype;
	if (scenario == 0 && win) {
		curr_payoff = payoff_matrix[1] - payoff_matrix[0] * 0.5;
		cum_payoff += payoff_matrix[1] - payoff_matrix[0] * 0.5;
	}
	else if (scenario == 0) {
		curr_payoff = - payoff_matrix[0] * 0.5;
		cum_payoff += - payoff_matrix[0] * 0.5;
	}
	else {
		curr_payoff = payoff_matrix[scenario];
		cum_payoff += payoff_matrix[scenario];
	}
	++ninterac;
}

double individual::logist(double otherBadge) { 
	return (1 / (1 + exp(-(get_badge() - otherBadge)))); 
}

void individual::set_phenotype(individual partner) {
	if (get_strat() == evaluator){
		double probHawk = logist(partner.get_badge());
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

void Reprod(vector<individual> &popT, int popsize, double mutRate,
	double mutSD, double baselineFit) {
	vector<individual> popTplus1(popsize);
	rnd::discrete_distribution payoff_dist(popsize);
	countGenotypes[0] = 0;
	countGenotypes[1] = 0;
	countGenotypes[2] = 0;
	BadgeMeanSd[0] = 0;
	BadgeMeanSd[1] = 0;
	alphaMeanSd[0] = 0;
	alphaMeanSd[1] = 0;
	betaMeanSd[0] = 0;
	betaMeanSd[1] = 0;
	for (vector<individual>::iterator itpop = popT.begin(); 
		itpop < popT.end(); ++itpop) {
		payoff_dist[itpop-popT.begin()] = baselineFit + 
			itpop->cum_payoff/itpop->ninterac;
		++countGenotypes[itpop->get_strat()];
		BadgeMeanSd[0] += itpop->get_badge();
		BadgeMeanSd[1] += pow(itpop->get_badge(), 2);
		alphaMeanSd[0] += itpop->get_alpha();
		alphaMeanSd[1] += pow(itpop->get_alpha(),2);
		betaMeanSd[0] += itpop->get_beta();
		betaMeanSd[1] += pow(itpop->get_beta(), 2);
	}
	for (vector<individual>::iterator itpopTplus1 = popTplus1.begin(); 
		itpopTplus1 < popTplus1.end(); ++itpopTplus1) {
		*itpopTplus1 = individual(popT[payoff_dist.sample()],mutRate,mutSD);
	}
	vector<individual>::iterator itpopTplus1 = popTplus1.begin();
	for (vector<individual>::iterator itpopT = popT.begin();
		itpopT < popT.end(); ++itpopT, ++itpopTplus1) {
		*itpopT = *itpopTplus1;
	}
}

void interactions(vector<individual> &population,int nint, int popsize,
	vector<double> payoff_matrix) {
	int ind1 = popsize;
	int ind2 = popsize;
	bool ind1win;
	countPhenotypes[0] = 0;
	countPhenotypes[1] = 0;
	for (int i = 0; i < nint*popsize; ++i) {
		while (ind1 == ind2) {
			ind1 = rnd::integer(popsize);
			ind2 = rnd::integer(popsize);
		}
		population[ind1].set_phenotype(population[ind2]);
		population[ind2].set_phenotype(population[ind1]);
		ind1win = rnd::binomial(1,
			population[ind1].logist(population[ind2].get_badge()));
		population[ind1].get_payoff(population[ind2], payoff_matrix,ind1win);
		population[ind2].get_payoff(population[ind1], payoff_matrix,!ind1win);
		ind1 = popsize, ind2 = popsize;
	}
}
double calcSd(double sum[], double invN) {
	return(sqrt(sum[1] * invN -
		pow((sum[0] * invN), 2)));
}

void printStats(int popsize,ofstream &output, int time, int seed) {
	double invertTotInt = 1/static_cast<double>(countPhenotypes[0] + 
		countPhenotypes[1]);
	double invertPopsize = 1/static_cast<double>(popsize);
	double badgSD = calcSd(BadgeMeanSd, invertPopsize);
	double alphaSD = calcSd(alphaMeanSd, invertPopsize);
	double betaSD = calcSd(betaMeanSd, invertPopsize);
	/*double badgSD = sqrt(BadgeMeanSd[1]*invertPopsize - 
		pow((BadgeMeanSd[0]*invertPopsize),2));*/
	output << seed << '\t';
	output << time << '\t';
	output << countGenotypes[hawk]      * invertPopsize << '\t';
	output << countGenotypes[dove]      * invertPopsize << '\t';
	output << countGenotypes[evaluator] * invertPopsize << '\t';
	output << countPhenotypes[hawk]     * invertTotInt << '\t';
	output << countPhenotypes[dove]     * invertTotInt << '\t';
	output << BadgeMeanSd[0]            * invertPopsize << '\t';
	output << badgSD << '\t';
	output << alphaMeanSd[0]            * invertPopsize << '\t';
	output << alphaSD << '\t';
	output << betaMeanSd[0]             * invertPopsize << '\t';
	output << betaSD ;
	output << endl;
	/*cout << seed << '\t';
	cout << time << '\t';
	cout << countGenotypes[hawk]        * invertPopsize << '\t';
	cout << countGenotypes[dove]        * invertPopsize << '\t';
	cout << countGenotypes[evaluator]   * invertPopsize << '\t';
	cout << countPhenotypes[hawk]       * invertTotInt << '\t';
	cout << countPhenotypes[dove]       * invertTotInt << '\t';
	cout << endl;*/

}

string create_filename(std::string filename, json param) {
	// name the file with the parameter specifications
	filename.append("_HH");
	filename.append(douts(param["payoff_matrix"][0]));
	filename.append("_HD");
	filename.append(douts(param["payoff_matrix"][1]));
	filename.append("_DH");
	filename.append(douts(param["payoff_matrix"][2]));
	filename.append("_DD");
	filename.append(douts(param["payoff_matrix"][3]));
	filename.append("_");
	std::string namParam = param["namParam"];
	filename.append(namParam);
	filename.append(douts(param[namParam]));
	filename.append(".txt");
	return(filename);
}
void initializeFile(ofstream &popOutput, json param) {
	std::string namedir = param["folder"];
	// 
	std::string namefile ="pop";
	namedir.append(namefile);
	string IndFile = create_filename(namedir, param);
	popOutput.open(IndFile.c_str());
	popOutput << "seed" << '\t';
	popOutput << "time" << '\t' << "freqGenHawks" << '\t' << "freqGenDove";
	popOutput << '\t' << "freqGenEval" << '\t' << "freqFenHawks" << '\t';
	popOutput << "freqFenDoves" << '\t' << "meanCue" << '\t' << "sdCue" << '\t';
	popOutput << "meanAlpha" << '\t' << "sdAlpha" << '\t' << "meanBeta" << '\t';
	popOutput << "sdBeta";
	popOutput << endl;
}

int main(int argc, _TCHAR* argv[]){

	mark_time(1);

	/*json param;
	param["totGen"]            = 1000;
	param["nRep"]              = 5;
	param["printGen"]          = 500;
	param["payoff_matrix"]     = {1.5,1,0,0.5};
	param["popSize"]           = 100;
	param["meanCue"]           = 20;
	param["sdCue"]             = 0.2;
	param["nInt"]              = 50;
	param["mutRate"]           = 0.001;
	param["baselineFit"]       = 1;
	param["namParam"]          = "sdCue";
	param["rangParam"]         = { 0.2,0.4,0.6,0.8,1 };
	param["folder"]            = "s:/quinonesa/simulations/Comp_cue/test_/";*/
	
		
	ifstream input(argv[1]);
	if (input.fail()) { cout << "JSON file failed" << endl; }
	nlohmann::json param = json::parse(input);

	string namParam = param["namParam"];

	vector<individual> population;

	for (json::iterator itParVal = param["rangParam"].begin();
		itParVal != param["rangParam"].end(); ++itParVal) {
		param[namParam] = *itParVal;
		ofstream popOutput;
		initializeFile(popOutput, param);
		for (int seed = 0; seed < param["nRep"]; ++seed) {
			cout << "sd=" << *itParVal << "	" << "seed=" << seed << endl;
			for (int popId = 0; popId < param["popSize"]; ++popId) {
				population.push_back(individual((strategy)rnd::integer(2)));
			}
			for (int generation = 0; generation < param["totGen"]; 
				++generation) {
				interactions(population, param["nInt"], param["popSize"],
					param["payoff_matrix"]);
				Reprod(population, param["popSize"], param["mutRate"],
					param["sdCue"], param["baselineFit"]);
				if (generation % static_cast<int>(param["printGen"]) == 0) {
					/*cout << "time=" << generation << endl;
					cout << "prinGen=" << param["printGen"] << endl;*/
					printStats(param["popSize"], popOutput, generation, seed);
				}
			}
			for (int popId = 0; popId < param["popSize"]; ++popId) {
				population.pop_back();
			}
		}
		popOutput.close();
	}
	
	mark_time(0);
	//wait_for_return();

	return 0;
}
