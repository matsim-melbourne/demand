package org.matsim.bendigoDemand.run;

import com.google.common.base.Preconditions;
import org.matsim.accessibillityDrtOptimizer.plan_filtering.PlanFilter;
import org.matsim.api.core.v01.Coord;
import org.matsim.api.core.v01.Scenario;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.Population;
import org.matsim.api.core.v01.population.PopulationWriter;
import org.matsim.application.MATSimAppCommand;
import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.network.NetworkUtils;
import org.matsim.core.router.TripStructureUtils;
import org.matsim.core.scenario.ScenarioUtils;
import picocli.CommandLine;

import java.util.List;

public class ChangeOriginToNearestHighway implements MATSimAppCommand {
    @CommandLine.Option(names = "--config", description = "config file", required = true)
    private String configPath;

    @CommandLine.Option(names = "--output", description = "directory to store filtered plans", required = true)
    private String outputDirectory;

    public static void main(String[] args) {
        new ChangeOriginToNearestHighway().execute(args);
    }

    @Override
    public Integer call() throws Exception {
        Config config = ConfigUtils.loadConfig(configPath, new MultiModeDrtConfigGroup());
        Scenario scenario = ScenarioUtils.loadScenario(config);
        Network network = scenario.getNetwork();
        Population inputPlans = scenario.getPopulation();

        for (Person person : inputPlans.getPersons().values()) {
            List<TripStructureUtils.Trip> trips = TripStructureUtils.getTrips(person.getSelectedPlan());
            Preconditions.checkArgument(trips.size() == 1, "Only trip based plan are supported. Check the input plans!");
            TripStructureUtils.Trip trip = trips.get(0);

            Link fromLink;
            if (trip.getOriginActivity().getLinkId() == null) {
                fromLink = NetworkUtils.getNearestLink(network, trip.getOriginActivity().getCoord());
            } else {
                fromLink = network.getLinks().get(trip.getOriginActivity().getLinkId());
            }
            Coord newOriginCoord = fromLink.getFromNode().getCoord();

            trip.getOriginActivity().setCoord(newOriginCoord);

            Link toLink;
            if (trip.getDestinationActivity().getLinkId() == null) {
                toLink = NetworkUtils.getNearestLink(network, trip.getDestinationActivity().getCoord());
            } else {
                toLink = network.getLinks().get(trip.getDestinationActivity().getLinkId());
            }
            Coord newDestinationCoord = toLink.getToNode().getCoord();

            trip.getDestinationActivity().setCoord(newDestinationCoord);
        }
        new PopulationWriter(inputPlans).write(outputDirectory);
        return 0;
    }
}

