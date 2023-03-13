Select *
From Portfolio..Covid_Deaths
Where continent is not null
ORDER BY 3,4

--Select *
--From Portfolio..CovidVaccinations
--ORDER BY 3,4

-- Select Data that we are going to be using

Select Location, date, total_cases, new_cases, total_deaths, population
From Portfolio..Covid_Deaths
Order by 1,2

-- Looking at Total Cases vs Total Deaths
-- Shows likelihood of dying if you contract covid in Canada.

Select Location, date, total_cases, total_deaths, (cast(total_deaths as int)/total_cases)*100
	as DeathPercentage
From Portfolio..Covid_Deaths
Where location like '%Canada%'
Order by 1,2

-- Looking at Total Cases vs Population
-- Shows what percentage of population got Covid

Select Location, date, Population, total_cases, (total_cases/population)*100
	as PercentPopulationInfected
From Portfolio..Covid_Deaths
--Where location like '%Canada%'
Where continent is not null
Order by 1,2

-- Looking at Countries with Highest Infection Rate compared to Population

Select Location, date, Population, MAX(cast(total_cases as int)) as HighestInfectionCount,
	Max((total_cases/population))*100 as PercentPopulationInfected
From Portfolio..Covid_Deaths
--Where location like '%Canada%'
--Where continent is not null
Group by Location, Population, date
Order by PercentPopulationInfected desc

-- Showing Countries with Highest Death Count per Population

Select Location, MAX(cast(Total_deaths as int)) as TotalDeathCount
From Portfolio..Covid_Deaths
--Where location like '%Canada%'
Where continent is not null
Group by Location
Order by TotalDeathCount desc

Select location, SUM(cast(new_deaths as int)) as TotalDeathCount
From Portfolio..Covid_Deaths
--Where location like '%states%'
Where continent is null
and location not in ('World', 'European Union', 'International',
'Upper middle income', 'Lower middle income', 'Low income', 'High income')
Group by location
Order by TotalDeathCount desc

-- LET'S BREAK THINGS DOWN BY CONTINENT 

Select continent, MAX(cast(Total_deaths as int)) as TotalDeathCount
From Portfolio..Covid_Deaths
--Where location like '%Canada%'
Where continent is not null
Group by continent
Order by TotalDeathCount desc

-- Showing contientns with the highest death count per population

Select continent, MAX(cast(Total_deaths as int)) as TotalDeathCount
From Portfolio..Covid_Deaths
--Where location like '%Canada%'
Where continent is not null
Group by continent
Order by TotalDeathCount desc

-- GLOBAL NUMBERS of Infection by Date

Select SUM(new_cases) as total_cases, SUM(cast(new_deaths as int)) as total_deaths,
	SUM(cast(new_deaths as int))/SUM(New_Cases)*100 as DeathPercentage
From Portfolio..Covid_Deaths
--Where location like '%Canada%'
Where continent is not null
--Group By date
Order by 1,2

-- Looking at Total Population vs Vaccinations per day

Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
	SUM(CONVERT(bigint, vac.new_vaccinations)) OVER (Partition by dea.location Order by dea.location,
	dea.date) as RollingPeopleVaccinated
	--,(RollingPeopleVaccinated/population)*100
From Portfolio..Covid_Deaths dea
Join Portfolio..Covid_Vaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
order by 2,3

-- USE CTE

With PopvsVac (Continent, Location, Date, Population, New_Vaccinations, RollingPeopleVaccinated)
as
(
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
	SUM(CONVERT(INT, vac.new_vaccinations)) OVER (Partition by dea.location Order by dea.location,
	dea.date) as RollingPeopleVaccinated
	--,(RollingPeopleVaccinated/population)*100
From Portfolio..Covid_Deaths dea
Join Portfolio..Covid_Vaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
--order by 2,3
)

Select *, (RollingPeopleVaccinated/Population)*100
From PopvsVac

--TEMP TABLE

DROP Table if exists #PercentPopulationVaccinated
Create Table #PercentPopulationVaccinated
(
Continent nvarchar(255),
Location nvarchar(255),
Date datetime,
Population numeric,
New_vaccinations numeric,
RollingPeopleVaccinated numeric
)

Insert into #PercentPopulationVaccinated
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
	SUM(CONVERT(int, vac.new_vaccinations)) OVER (Partition by dea.location Order by dea.location,
	dea.date) as RollingPeopleVaccinated
	--,(RollingPeopleVaccinated/population)*100
From Portfolio..CovidDeaths dea
Join Portfolio..CovidVaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
--where dea.continent is not null
--order by 2,3

Select *, (RollingPeopleVaccinated/Population)*100
From #PercentPopulationVaccinated

-- Creating View to store data for later visualizations

Create View PercentPopulationVaccinated as 
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
	SUM(CONVERT(int, vac.new_vaccinations)) OVER (Partition by dea.location Order by dea.location,
	dea.date) as RollingPeopleVaccinated
	--,(RollingPeopleVaccinated/population)*100
From Portfolio..CovidDeaths dea
Join Portfolio..CovidVaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
-- order by 2,3

Select *
From PercentPopulationVaccinated