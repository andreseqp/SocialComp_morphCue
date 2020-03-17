
/*=============================================================================
cue_comp.cpp
===============================================================================
Main file of a model exploring the evolution communication systems where 
individuals learn to respond to a signal. Individuals play the 
traditional hawk-dove game. 


Written by :

Andr�s E.Qui�ones
Posdoctoral researcher
Departamento de ciencias biol�gicas
Universidad de los Andes
Bogot� Colombia

Start date :
11 June 2018
=============================================================================*/


#include <stdio.h>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <math.h>	
#include <vector>
#include <omp.h>
//#include "tchar.h"   //Eliminate for g++
#include "../Cpp/Routines/C++/RandomNumbers/random.h"
#include "../Cpp/json.hpp"       
#include "../Cpp/Routines/C++/RandomNumbers/utils.h"
// Header for reading and using JSON files see https://github.com/nlohmann/json

using namespace std;
using json = nlohmann::json;

// Classes

enum strategy { hawk, dove, evaluator};

class printingObj;

class individual {
	public:
		individual(strategy genotype_, double QualStDv,
			double alphaBadge_, double alphaCI, 
			double alphaAI, double gammaI, double sigmaSqI,	int nCenters_,
			double initCrit, double initAct );
		individual(individual& mother, double QualStDv,
			double mutRate,double mutSD, int mutType, double initCrit, 
			double initAct);
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
		double get_quality() {
			return(quality);
		}
		double  get_feat(bool act,int id){
			if(act)	return(featWeightsAct[id]);
			else return (featWeightsCrit[id]);
		}
		int get_nCenter() {
			return(nCenters);
		}
		int set_phenotype(individual partner, printingObj &localPrint);
		void get_payoff(individual partner, vector<double> param, bool win);
		void set_Badge(double stdDev);
		bool Winfight(double otherQuality, double strQual);
		strategy mutateStr(strategy genotype,double mutRate,int mutType);
		double mutateDoub(double value, double mutRate, double mutSD);
		void calcRespValPref(individual partner);
		void update();
		bool viability(double alphaCost, double betaCost);
	private:
		int nCenters;
		// number of ranges in which the morphological trait range is divided
		vector<double> featWeightsCrit;
		// vector of weights to fit the state value function. 
		// Each corresponds to one value in the morphological trait range
		vector <double> featWeightsAct;
		// vector of weights to fit the action function. 
		// Each corresponds to one value in the morphological trait range
		vector <double> centers;
		// Numerical value in the morphological trait range that corresponds 
		// to the weights in FeatWeightsCrit anf featWeightAct
		vector <double> responses;
		// Response triggered by each weight corrected by distance accordig to RBF
		double valueT; // estimated value for state at time t
		double preferenceT;  // estimated preference for acting as hawk at time t
		double sigmaSq; 
		// degree of generalization
		double alphaCrit;
		// speed of learning for the critic
		double alphaAct;
		// speed of learning for the actor
		double gamma;
		// importance of future rewards 
		strategy genotype;
		double own_badge;
		double quality;
		double alphaBadge;
		double betaBadge;
};

// set quality and badge of status
void individual::set_Badge(double stdDev=0.1) {
	if (stdDev>1)
	{
		quality = rnd::uniform();
	}
	else {
		quality = rnd::normal(0.5, stdDev);
		clip_range(quality, 0, 1);
	}
	own_badge = 1 / (1 + exp(alphaBadge - betaBadge * quality));
}

// constructors
individual::individual(strategy genotype_=hawk, double QualStDv = 0.1,
	double alphaBadge_=0, double alphaCI = 0.05, 
	double alphaAI = 0.05, double gammaI = 0, double sigmaSqI = 0.01, 
	int nCenters_=6,double initCrit=0,double initAct=0) {
	nCenters = nCenters_;
	alphaBadge = alphaBadge_;
	betaBadge = alphaBadge_*2;
	genotype = genotype_;
	set_Badge(QualStDv);
	alphaAct = alphaAI, alphaCrit = alphaCI, gamma = gammaI, sigmaSq = sigmaSqI;
	double interv = 1 / (static_cast<double>(nCenters)-1);
	for (int i = 0; i < nCenters; i++)	{
		centers.emplace_back(interv * i);
		featWeightsAct.emplace_back(initAct);
		featWeightsCrit.emplace_back(initCrit);
		responses.emplace_back(0);
	}
	curr_payoff = 0, cum_payoff = 0, ninterac = 0, valueT = 0;
	preferenceT=0;
}

individual::individual(individual& mother, double QualStDv,
	double mutRate, double mutSD, int mutType, 
	double initCrit = 0, double initAct = 0) {
	genotype = mutateStr(mother.genotype,mutRate,mutType);
	nCenters = mother.nCenters;
	alphaBadge = mutateDoub(mother.alphaBadge,mutRate,mutSD);
	betaBadge = mutateDoub(mother.betaBadge,mutRate,mutSD);
	set_Badge(QualStDv); 
	curr_payoff = 0, cum_payoff = 0, ninterac = 0, valueT = 0, 
	preferenceT=0;
	alphaAct = mother.alphaAct;
	alphaCrit = mother.alphaCrit;
	gamma = mother.gamma;
	sigmaSq = mother.sigmaSq;
	double interv = 1 / (static_cast<double>(nCenters) - 1);
	for (int i = 0; i < nCenters; i++) {
		centers.emplace_back(interv * i);
		featWeightsAct.emplace_back(initAct);
		featWeightsCrit.emplace_back(initCrit);
		responses.emplace_back(0);
	}
}

void individual::calcRespValPref(individual partner) {
	double totValue = 0;
	double totPref = 0;
	for (int countCenters = 0; countCenters < nCenters; ++countCenters) {
		responses[countCenters] = 
			exp(-(pow(abs(partner.get_badge() - centers[countCenters]),2))
			/ (2 * sigmaSq));
		totValue += responses[countCenters] * featWeightsCrit[countCenters];
		totPref += responses[countCenters] * featWeightsAct[countCenters];
	}
	valueT = totValue;
	preferenceT = totPref;
}

double logist(double value1, double value2, double beta=1,double alpha=0) {
	return (1 / (1 + exp(alpha-beta*(value1 - value2))));
}

void individual::update() {
	// change estimated value according to current reward and 
	// estimates of future state-action pair
	double delta = curr_payoff - valueT;
	double p0 = logist(preferenceT,0);
	double eligVec = 0;
	if (phenotype==0) {
		// if phenotype is hawk
		eligVec = -p0;
	}
	else {
		// if phenotype is dove
		eligVec = (1 - p0);
	}
	for (int countCent = 0; countCent < nCenters; ++countCent) {
		featWeightsCrit[countCent] += alphaCrit * delta*
			responses[countCent];
		featWeightsAct[countCent] += alphaAct * delta*eligVec*
			responses[countCent];
	}
}

strategy individual::mutateStr(strategy genotype,double mutRate,int mutType) {
	if (mutType > 0) {
		if (rnd::uniform() < mutRate) {
			strategy newgenotype = (strategy)rnd::integer(mutType);
			// set intput to random generator to 3, to include evaluators
			// set intput to random generator to 2, for simple hawk-dove game
			return(newgenotype);
		}
		else {
			return(genotype);
		}
	}
	else return(genotype);
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

bool individual::Winfight(double otherQuality,double strQual) {
	return(rnd::bernoulli(logist(get_quality(), otherQuality, strQual)));
}


bool individual::viability(double alphaCost,double betaCost) {
	/*cout << logist(quality, own_badge, betaCost, -alphaCost) << endl;
	cout << rnd::bernoulli(logist(quality, own_badge, betaCost, -alphaCost)) << endl;*/
	return(rnd::bernoulli(logist(quality,own_badge,betaCost,-alphaCost)));
}

class printingObj {
	
public:
	printingObj(int nSamples, int nInt, int printLearnInt,
		int nCenters);
	void recordInd(int idSamInd, individual focal);
	int countGenotypes[3];
	// records the frequency of genotypes for evolutionary dyn
	int countPhenotypes[2];
	// records the frequency of phenotypes for evolutionary dyn
	int countIntTypes[3];
	// records the frequency of interaction types for evolutionary dyn
	double BadgeMeanSd[2];
	// records the distributions of badge sizes for evolutionary dyn
	double alphaMeanSd[2];
	// records the distribution of alpha in the reac norm for evolutionary dyn
	double betaMeanSd[2];
	// records the distribution of betas in the reac norm for evolutionary dyn
	double featActMean[20];
	// records the distribution of Actor features at the end of learning for evolutionary dyn
	double featCritMean[20];
	// records the distribution of Critic features at the end of learning for evolutionary dyn
	//vector<vector<int> > countIntTypesGen;
	// records the frequency of interaction types for learning dyn
	vector<int> sampledInd;
	// Which inds are sampled for learning dyn
	vector<int>  interacCount;
	vector<int> counterRecords;
	// N interactions of sampled inds for learn dyn
	vector<vector<vector<double> > > actFeatHistory;
	// Act feat. weigths of sampled inds for learn dyn
	vector<vector<vector<double> > > critFeatHistory;
	// Crit feat. weigths of sampled inds for learn dyn
};

printingObj::printingObj(int nSamples, int nInt, int printLearnInt,
	int nCenters = 6) {
	int nInterRecords = 1 + 4 * nInt / printLearnInt;
	for (int countRecords = 0; countRecords < nInterRecords; ++countRecords) {
		/*countIntTypesGen.emplace_back(0);
		countIntTypesGen[countRecords].emplace_back(0);
		countIntTypesGen[countRecords].emplace_back(0);*/
		interacCount.emplace_back(countRecords * printLearnInt);
		actFeatHistory.emplace_back(0);
		critFeatHistory.emplace_back(0);
		for (int countSamples = 0; countSamples < nSamples; ++countSamples) {
			if (countRecords == 0) {
				sampledInd.emplace_back(0), counterRecords.emplace_back(0);
			}
			actFeatHistory[countRecords].emplace_back(0);
			critFeatHistory[countRecords].emplace_back(0);
			for (int countCenters = 0; countCenters < nCenters; ++countCenters) {
				actFeatHistory[countRecords][countSamples].emplace_back(0);
				critFeatHistory[countRecords][countSamples].emplace_back(0);
			}
		}
	}
}

void printingObj::recordInd(int idSamInd, individual focal) {
	for (int countCenters = 0; countCenters < focal.get_nCenter(); ++countCenters) {
		actFeatHistory[counterRecords[idSamInd]][idSamInd][countCenters]=
			focal.get_feat(1,countCenters);
		critFeatHistory[counterRecords[idSamInd]][idSamInd][countCenters] =
			focal.get_feat(0, countCenters);
	}
	++counterRecords[idSamInd];
}

int individual::set_phenotype(individual partner, printingObj& localPrint) {
	if (get_strat() == evaluator) {
		calcRespValPref(partner);
		phenotype = static_cast<strategy>(rnd::bernoulli(logist(preferenceT, 0)));
	}
	else {
		phenotype = get_strat();
	}
	++localPrint.countPhenotypes[phenotype];
	return(phenotype);
}

void Reprod(vector<individual> &popT, int popsize, double mutRate,
	double mutSD, double baselineFit, int mutType, double QualStDv,
	double initCrit, double initAct,double alphaCost, double betaCost) {
	vector<individual> popTplus1(popsize);
	rnd::discrete_distribution payoff_dist(popsize);
	
	for (vector<individual>::iterator itpop = popT.begin(); 
		itpop < popT.end(); ++itpop) {
		payoff_dist[itpop-popT.begin()] = baselineFit + 
			itpop->cum_payoff/itpop->ninterac;
	}
	vector<individual>::iterator itpopTplus1 = popTplus1.begin();
	while (	itpopTplus1 < popTplus1.end()) {
		*itpopTplus1 = individual(popT[payoff_dist.sample()], QualStDv, mutRate,
			mutSD, mutType, initCrit, initAct);
		if (betaCost == 0) ++itpopTplus1;
		else if (itpopTplus1->viability(alphaCost, betaCost)) {
			++itpopTplus1;
		}
	}
	popT = popTplus1;
	/*vector<individual>::iterator itpopTplus1 = popTplus1.begin();
	for (vector<individual>::iterator itpopT = popT.begin();
		itpopT < popT.end(); ++itpopT, ++itpopTplus1) {
		*itpopT = *itpopTplus1;
	}*/
}

void get_stats(vector<individual> &popT, int popsize, printingObj &localPrint,int nFeat=6){
	localPrint.countGenotypes[0] = 0;
	localPrint.countGenotypes[1] = 0;
	localPrint.countGenotypes[2] = 0;
	localPrint.BadgeMeanSd[0] = 0;
	localPrint.BadgeMeanSd[1] = 0;
	localPrint.alphaMeanSd[0] = 0;
	localPrint.alphaMeanSd[1] = 0;
	localPrint.betaMeanSd[0] = 0;
	localPrint.betaMeanSd[1] = 0;
	for(int countFeat = 0; countFeat<nFeat;++countFeat){
		localPrint.featActMean[countFeat] = 0, localPrint.featCritMean[countFeat] = 0;
	}
	for (vector<individual>::iterator itpop = popT.begin(); 
		itpop < popT.end(); ++itpop) {
		++localPrint.countGenotypes[itpop->get_strat()];
		localPrint.BadgeMeanSd[0] += itpop->get_badge();
		localPrint.BadgeMeanSd[1] += pow(itpop->get_badge(), 2);
		localPrint.alphaMeanSd[0] += itpop->get_alpha();
		localPrint.alphaMeanSd[1] += pow(itpop->get_alpha(),2);
		localPrint.betaMeanSd[0] += itpop->get_beta();
		localPrint.betaMeanSd[1] += pow(itpop->get_beta(), 2);
		for(int countFeat = 0; countFeat<nFeat;++countFeat){
			localPrint.featActMean[countFeat] += itpop->get_feat(1,countFeat);
			localPrint.featCritMean[countFeat] += itpop->get_feat(0, countFeat);
		}
	}
}

void printLearnDynamics(ofstream &genoutput, vector<individual> &pop,
	int generat, int seed, printingObj &localPrint) {
	for (int cIntRecords = 0; cIntRecords < localPrint.interacCount.size();
		++cIntRecords) {
		for (int cSampled = 0; cSampled < localPrint.sampledInd.size(); ++cSampled) {
			genoutput << seed << '\t' << generat << '\t' <<
				localPrint.interacCount[cIntRecords] << '\t' <<
				localPrint.sampledInd[cSampled] << '\t';
				/*genoutput << localPrint.countIntTypesGen[cIntRecords][0] << '\t' <<
				localPrint.countIntTypesGen[cIntRecords][1] << '\t' <<
				localPrint.countIntTypesGen[cIntRecords][2] << '\t';*/
			for (int countFeat = 0; countFeat < pop[0].get_nCenter();
				++countFeat) {
				genoutput << localPrint.actFeatHistory[cIntRecords][cSampled][countFeat]
					<< '\t'
					<< localPrint.critFeatHistory[cIntRecords][cSampled][countFeat]
					<< '\t';
			}
			genoutput << pop[localPrint.sampledInd[cSampled]].get_quality() << '\t' <<
				pop[localPrint.sampledInd[cSampled]].get_strat() << '\t' <<
				pop[localPrint.sampledInd[cSampled]].get_alpha() << '\t' <<
				pop[localPrint.sampledInd[cSampled]].get_beta() << '\t' <<
				pop[localPrint.sampledInd[cSampled]].get_badge() << '\t';
			genoutput << endl;
		}
	}
	
}

void interactions(vector<individual>& population, ofstream& genoutput, int nint,
	vector<double> payoff_matrix, double strQual, bool trackPopLearn,
	int printLearnInt, int sampleSize, int generat, int seed, int nIntGroup,   
	printingObj &localPrint) {
	int ind1 = population.size();
	int ind2 = population.size();
	int intType = 0;
	bool ind1win = 0;
	vector<int> sample;
	vector<int>::iterator itSamp1 = sample.begin();
	//cout << "track " << generat << endl;
	if (trackPopLearn) {
		localPrint.countPhenotypes[0] = 0, localPrint.countPhenotypes[1] = 0;
		localPrint.countIntTypes[0] = 0, localPrint.countIntTypes[1] = 0;
		localPrint.countIntTypes[2] = 0;
		/*localPrint.countIntTypesGen[0];
		localPrint.countIntTypesGen[1], localPrint.countIntTypesGen[2] = 0;*/
		for (int countSam = 0; countSam < sampleSize; ++countSam) {
			sample.emplace_back(rnd::integer(population.size()));
			localPrint.sampledInd[countSam] = sample[countSam];
			localPrint.counterRecords[countSam] = 0;
		}
	}
	for (int i = 0; i < nint*population.size(); ++i) {
		ind1 = rnd::integer(population.size());
		ind2 = ind1+1+rnd::integer(nIntGroup-1);
		if (ind2 >= population.size()) ind2 = ind2 - population.size();
		intType = 0;
		intType += population[ind1].set_phenotype(population[ind2],localPrint);
		intType += population[ind2].set_phenotype(population[ind1],localPrint);
		if (trackPopLearn) { 
			//cout << "trackpoplearn " << generat << endl;
			int foundInd = 0;
			++localPrint.countIntTypes[intType];
			//++localPrint.countIntTypesGen[intType];
			itSamp1 = find(sample.begin(), sample.end(), ind1);
			if (itSamp1 != sample.end() &&
				(population[*itSamp1].ninterac % printLearnInt) == 0) {
				localPrint.recordInd(itSamp1 - sample.begin(), population[*itSamp1]);
					++foundInd;
			}
			itSamp1 = find(sample.begin(), sample.end(), ind2);
			if (itSamp1 != sample.end() &&
				(population[*itSamp1].ninterac % printLearnInt) == 0) {
				localPrint.recordInd(itSamp1 - sample.begin(), population[*itSamp1]);
				++foundInd;
			}
			/*if (foundInd > 0) {
				localPrint.countIntTypesGen[0]=0, localPrint.countIntTypesGen[1]=0,
				localPrint.countIntTypesGen[2] = 0;
			}*/
		}
		//cout << "win " << generat << endl;
		ind1win = population[ind1].Winfight(population[ind2].get_quality(),
			strQual);
		//cout << "payoff " << generat << endl;
		population[ind1].get_payoff(population[ind2], payoff_matrix,ind1win);
		population[ind2].get_payoff(population[ind1], payoff_matrix,!ind1win);
		/*cout << "update " << generat << endl;*/
		population[ind1].update();
		population[ind2].update();
		ind1 = population.size(), ind2 = population.size();
	}
}

double calcSd(double sum[], double invN) {
	return(sqrt(sum[1] * invN -
		pow((sum[0] * invN), 2)));
}

void printStats(int popsize,ofstream &evolOutput, 
	int time, int seed,	printingObj localPrint,int nFeat = 5) {
	double invertTotInt = 1/(static_cast<double>(localPrint.countPhenotypes[0]) +
		static_cast<double>	(localPrint.countPhenotypes[1]));
	double invertPopsize = 1/static_cast<double>(popsize);
	double invertNlearners = 0;
	if (localPrint.countGenotypes[2] != 0) {
		invertNlearners = 1 / static_cast<double>(localPrint.countGenotypes[2]);
	}
	else {
		invertNlearners = 0;
	}
	double badgSD = calcSd(localPrint.BadgeMeanSd, invertPopsize);
	double alphaSD = calcSd(localPrint.alphaMeanSd, invertPopsize);
	double betaSD = calcSd(localPrint.betaMeanSd, invertPopsize);
	evolOutput << seed << '\t';
	evolOutput << time << '\t';
	evolOutput << localPrint.countGenotypes[hawk]      * invertPopsize << '\t';
	evolOutput << localPrint.countGenotypes[dove]      * invertPopsize << '\t';
	evolOutput << localPrint.countGenotypes[evaluator] * invertPopsize << '\t';
	evolOutput << localPrint.countPhenotypes[hawk]     * invertTotInt << '\t';
	evolOutput << localPrint.countPhenotypes[dove]     * invertTotInt << '\t';
	evolOutput << (double)(localPrint.countIntTypes[0]) * 2 * invertTotInt << '\t';
	evolOutput << (double)(localPrint.countIntTypes[1]) * 2 * invertTotInt << '\t';
	evolOutput << (double)(localPrint.countIntTypes[2]) * 2 * invertTotInt << '\t';
	evolOutput << localPrint.BadgeMeanSd[0]            * invertPopsize << '\t';
	evolOutput << badgSD << '\t';
	evolOutput << localPrint.alphaMeanSd[0]            * invertPopsize << '\t';
	evolOutput << alphaSD << '\t';
	evolOutput << localPrint.betaMeanSd[0]             * invertPopsize << '\t';
	evolOutput << betaSD <<  '\t';
	for (int countFeat = 0; countFeat < nFeat; ++countFeat) {
		evolOutput << localPrint.featActMean[countFeat] * invertNlearners << '\t';
		evolOutput << localPrint.featCritMean[countFeat] * invertNlearners << '\t';
	}
	evolOutput << endl;
	
	//for (int countFeat = 0; countFeat < nFeat; ++countFeat) {
	//	cout << featActMean[countFeat] * invertPopsize << '\t';
	//	cout << featCritMean[countFeat] * invertPopsize << '\t';
	//}
	//cout << endl;
}

void printPopSample(vector<individual> &population, ofstream &popOutput,
	int time, int seed, int sampleSize, int nFeat = 5) {
	int sample;
	for (int countSample = 0; countSample < sampleSize; ++countSample) {
		sample = rnd::integer(population.size());
		popOutput << seed << '\t';
		popOutput << time << '\t';
		popOutput << sample << '\t';
		popOutput << population[sample].get_quality() << '\t';
		popOutput << population[sample].get_strat() << '\t';
		popOutput << population[sample].get_alpha() << '\t';
		popOutput << population[sample].get_beta() << '\t';
		popOutput << population[sample].get_badge() << '\t';
		popOutput << population[sample].ninterac << '\t';
		for (int countFeat = 0; countFeat < nFeat; ++countFeat) {
			popOutput << population[sample].get_feat(1,countFeat) << '\t';
			popOutput << population[sample].get_feat(0,countFeat) << '\t';
		}
		popOutput << endl;
	}
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
void initializeFiles(ofstream &evolOutput, //ofstream &popOutput, 
	ofstream &indOutput, json param) {
	std::string filename = param["folder"];
	filename.append("evolLearn");
	// File to print evolutionary dynamics
	std::string evolFile = create_filename(filename,param);
	/*std::string namefile ="popLearn";
	namedir.append(namefile);*/
	evolOutput.open(evolFile.c_str());
	evolOutput << "seed" << '\t';
	evolOutput << "time" << '\t' << "freqGenHawks" << '\t' << "freqGenDove";
	evolOutput << '\t' << "freqGenEval" << '\t' << "freqFenHawks" << '\t';
	evolOutput << "freqFenDoves" << '\t' << "freqHH" << '\t' << "freqHD" << '\t';
	evolOutput << "freqDD" << '\t' << "meanCue" << '\t' << "sdCue" << '\t';
	evolOutput << "meanAlpha" << '\t' << "sdAlpha" << '\t' << "meanBeta" << '\t';
	evolOutput << "sdBeta" << '\t';
	for(int countFeat=0;countFeat<param["nCenters"];++countFeat){
		evolOutput << "WeightAct_" + itos(countFeat) << '\t';
		evolOutput << "WeightCrit_" + itos(countFeat) << '\t';
	}
	evolOutput << endl;
	// File to print a sample of populations
	/*std::string filename1 = param["folder"];
	filename1.append("popLearn");
	std::string popFile = create_filename(filename1, param);
	popOutput.open(popFile.c_str());
	popOutput << "seed" << '\t';
	popOutput << "time" << '\t' << "idInd" << '\t' << "Quality";
	popOutput << '\t' << "genotype" << '\t' << "alpha" << '\t';
	popOutput << "beta" << '\t' << "Badge" << '\t' << "nInteract" << '\t';
	for (int countFeat = 0; countFeat < nFeat; ++countFeat) {
		popOutput << "WeightAct_" + itos(countFeat) << '\t';
		popOutput << "WeightCrit_" + itos(countFeat) << '\t';
	}
	popOutput << endl;*/
	// File to print a learning dynamics within a generation
	std::string filename1 = param["folder"];
	filename1.append("indLearn");
	std::string IndFile = create_filename(filename1, param);
	indOutput.open(IndFile.c_str());
	indOutput << "seed" << '\t' << "time" << '\t' << "nInteract" << '\t' 
		<< "indId" << '\t';
		/*indOutput << "nint_HH" << '\t' <<
		"nint_HD" << '\t' << "nint_DD" << '\t';*/
	for (int countFeat = 0; countFeat < param["nCenters"];
		++countFeat) {
		indOutput << "WeightAct_" + itos(countFeat) << '\t'
			<< "WeightCrit_" + itos(countFeat) << '\t';
	}
	indOutput << "Quality" << '\t' << "genotype" << '\t' << "alpha" << '\t';
	indOutput << "beta" << '\t' << "Badge" << '\t' ;
	indOutput << endl;
	//for (int countFeat = 0; countFeat < nFeat; ++countFeat) {
	//	cout << "WeightAct_" + itos(countFeat) << '\t';
	//	cout << "WeightCrit_" + itos(countFeat) << '\t';
	//}
	//cout << endl;
}

int main(int argc, char* argv[]){

	mark_time(1);

	//uncomment for debugging
	//json param;
	//param["totGen"]            = 50;   // Total number of generations
	//param["nRep"]              = 10;     // Number of replicates
	//param["printGen"]          = 1000;     // How often data is printed	
	//param["printLearn"]        = 3500;	  // how often learning dyn are printed
	//param["printLearnInt"]     = 3500;   // How often are learning parameters printed
	//param["init"]              = {0,0,1};        //Initial frequencies
	//param["payoff_matrix"]     = {1.5,1,0,0.5};  
	//param["popSize"]           = 1000;
	//param["MutSd"]             = 0.3;
	//param["nInt"]              = 2000;    // Number of interactions per individual
	//param["mutRate"]           = 0.05;
	//param["strQual"]           = 10;
	//param["baselineFit"]       = 2;
	//param["mutType"]		     = 0;  
	//// How many strategies are introduced by mutation
	//param["sampleSize"]        = 10; 
	//param["alphaBad"]			 = 0;
	//param["betaBad"]			 = 0;
	//param["alphaCrit"]     	 = 0.01;
	//param["alphaAct"]     	 = 0.01;
	//param["sigSq"]        	 = 0.01;
	//param["nCenters"]     	 = 6;
	//param["initCrit"]          = 0;
	//param["initAct"]           = 0;
	//param["QualStDv"]          = 1.1;
	//param["nIntGroup"]		 = 1000;
	//param["initAct"]			 =0;
	//param["betCost"]           = 0;
	//param["alphCost"]			 = 3;
	//param["namParam"]          = "nIntGroup";  
	//// which parameter to vary inside the program
	//param["rangParam"]         = {  10, 1000 }; 
	//// range in which the paramenter varies
	//param["folder"]            = "I:/Projects/SocialComp_morphCue/Simulations/test_/";
	
	nlohmann::json* pointParam;
		
	// Comment for debugging
	ifstream input(argv[1]);
	if (input.fail()) { cout << "JSON file failed" << endl; }
	nlohmann::json param = json::parse(input);

	pointParam = &param;

	string namParam = param["namParam"];

	
	for (json::iterator itParVal = param["rangParam"].begin();
		itParVal != param["rangParam"].end(); ++itParVal) {
		param[namParam] = *itParVal;
		ofstream  evolOutput, indOutput;//popOutput,
		initializeFiles(evolOutput,indOutput,param);//popOutput,
		#pragma omp parallel for firstprivate(pointParam)
		for (int seed = 0; seed < int(param["nRep"]); ++seed) {
			nlohmann::json paramL = *pointParam;
			// if (seed ==0) cout << omp_get_num_threads() << endl;
			#pragma omp critical
			{
				cout << paramL["namParam"] << "=" << *itParVal << "	" <<
					"seed=" << seed << endl;
			}
			vector<individual> population;
			population.reserve(paramL["popSize"]);
			printingObj localPrint = printingObj(paramL["sampleSize"], paramL["nInt"],
				int(paramL["printLearnInt"]), int(paramL["nCenters"]));
			// intial conditions
			rnd::discrete_distribution initFreq(3);
			for (json::iterator initIt = paramL["init"].begin();
				initIt != paramL["init"].end(); ++initIt) {
				initFreq[initIt - paramL["init"].begin()] = *initIt;
			}
			for (int popId = 0; popId < paramL["popSize"]; ++popId) {
				population.push_back(individual((strategy)initFreq.sample(),
					paramL["QualStDv"], paramL["alphaBad"],	
					paramL["alphaCrit"],	paramL["alphaAct"],0, paramL["sigSq"], 
					paramL["nCenters"],paramL["initCrit"],paramL["initAct"]));
			}
			for (int generation = 0; generation < paramL["totGen"]; 
				++generation) {
				//cout << "Interactions " << generation << endl;
				interactions(population, indOutput, paramL["nInt"],
					paramL["payoff_matrix"], paramL["strQual"],
					generation % static_cast<int>(paramL["printLearn"]) == 0,
					paramL["printLearnInt"], paramL["sampleSize"], generation, seed, 
					paramL["nIntGroup"],localPrint);
				#pragma omp critical
				{
					if (generation % static_cast<int>(paramL["printGen"]) == 0) {
						get_stats(population, paramL["popSize"],localPrint, paramL["nCenters"]);
						printStats(paramL["popSize"], evolOutput, generation, seed, 
							localPrint, paramL["nCenters"]);
						printLearnDynamics(indOutput,population,generation,seed,localPrint);
					}
					/*printPopSample(population, popOutput, generation, seed,
					paramL["sampleSize"],paramL["nCenters"]);*/
				}
				Reprod(population, paramL["popSize"], paramL["mutRate"],
					paramL["MutSd"], paramL["baselineFit"],paramL["mutType"],
					paramL["QualStDv"],paramL["initCrit"], paramL["initAct"], 
					paramL["alphCost"], 
					paramL["betCost"]);
				
			}
		
		}
		//popOutput.close();
		evolOutput.close();
		indOutput.close();
	}
	
	mark_time(0);
	//wait_for_return();

	return 0;
}
