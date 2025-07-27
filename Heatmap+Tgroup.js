// -------------------- Heatmap Section --------------------
const mapWidth = 1000, mapHeight = 600;
const svgMap = d3.select("svg").attr("width", mapWidth).attr("height", mapHeight);
const projection = d3.geoMercator().scale(130).translate([mapWidth / 2, mapHeight / 1.5]);
const path = d3.geoPath().projection(projection);
const tooltip = d3.select(".tooltip");

Promise.all([ 
    d3.json("https://raw.githubusercontent.com/holtzy/D3-graph-gallery/master/DATA/world.geojson"),
    d3.csv("g_data.csv") 
]).then(([world, data]) => {
    const incidentCountsByYear = {};
    data.forEach(d => {
        if (!incidentCountsByYear[d.iyear]) {
            incidentCountsByYear[d.iyear] = {};
        }
        incidentCountsByYear[d.iyear][d.country_txt] = +d.incident_count;
    });

    const maxIncidents = d3.max(data, d => +d.incident_count);
    const colorScale = d3.scaleSequential(d3.interpolateReds).domain([0, maxIncidents]);

    // Use d3.geoJson() to work with GeoJSON data
    svgMap.selectAll(".country")
        .data(world.features)  // No need to call topojson.feature for GeoJSON
        .enter().append("path")
        .attr("class", "country")
        .attr("d", path)
        .attr("fill", d => {
            const countryName = d.properties.name;
            return incidentCountsByYear[2020] && incidentCountsByYear[2020][countryName]
                ? colorScale(incidentCountsByYear[2020][countryName])
                : "#ccc";
        })
        .on("mouseover", (event, d) => {
            const countryName = d.properties.name;
            const year = +d3.select("#yearSlider").property("value");  // Get the selected year
            const count = incidentCountsByYear[year] ? incidentCountsByYear[year][countryName] : 0;

            tooltip.style("opacity", 1)
                .html(`<strong>${countryName}</strong><br>Incidents: ${count}`)
                .style("left", (event.pageX + 10) + "px")
                .style("top", (event.pageY - 10) + "px");
        })
        .on("mousemove", (event) => {
            tooltip.style("left", (event.pageX + 10) + "px")
                .style("top", (event.pageY - 10) + "px");
        })
        .on("mouseout", () => tooltip.style("opacity", 0));

    d3.select("#yearSlider").on("input", function() {
        const year = +this.value;
        d3.select("#yearLabel").text(year);
        updateMapForYear(year);
    });

    function updateMapForYear(year) {
        svgMap.selectAll(".country")
            .transition()
            .duration(1000)
            .attr("fill", d => {
                const countryName = d.properties.name;
                return incidentCountsByYear[year] && incidentCountsByYear[year][countryName]
                    ? colorScale(incidentCountsByYear[year][countryName])
                    : "#ccc";
            });
    }

    const legendWidth = 300, legendHeight = 20;
    const legendX = mapWidth - legendWidth - 50, legendY = mapHeight - 50;

    const legend = svgMap.append("g")
        .attr("class", "legend")
        .attr("transform", `translate(${legendX}, ${legendY})`);

    const defs = svgMap.append("defs");
    const linearGradient = defs.append("linearGradient")
        .attr("id", "legend-gradient")
        .attr("x1", "0%").attr("y1", "0%")
        .attr("x2", "100%").attr("y2", "0%");

    linearGradient.selectAll("stop")
        .data([ 
            { offset: "0%", color: colorScale(0) }, 
            { offset: "50%", color: colorScale(maxIncidents / 2) }, 
            { offset: "100%", color: colorScale(maxIncidents) } 
        ])
        .enter().append("stop")
        .attr("offset", d => d.offset)
        .attr("stop-color", d => d.color);

    legend.append("rect")
        .attr("width", legendWidth)
        .attr("height", legendHeight)
        .style("fill", "url(#legend-gradient)");

    const legendScale = d3.scaleLinear()
        .domain([0, maxIncidents])
        .range([0, legendWidth]);

    const legendAxis = d3.axisBottom(legendScale)
        .ticks(5)
        .tickFormat(d => Math.round(d));

    legend.append("g")
        .attr("transform", `translate(0, ${legendHeight})`)
        .call(legendAxis);

    legend.append("text")
        .attr("x", legendWidth / 2)
        .attr("y", -10)
        .attr("text-anchor", "middle")
        .text("Number of Incidents");

}).catch(error => console.error(error));


// -------------------- Top 10 Terrorist Groups Section --------------------
const margin = {top: 20, right: 30, bottom: 40, left: 150};
const chartWidth = 1000 - margin.left - margin.right;
const chartHeight = 600 - margin.top - margin.bottom;

const svgChart = d3.select("#chart")
                  .append("svg")
                  .attr("width", chartWidth + margin.left + margin.right)
                  .attr("height", chartHeight + margin.top + margin.bottom)
                  .append("g")
                  .attr("transform", `translate(${margin.left},${margin.top})`);

d3.csv("barchart.csv").then(function(data) {
    const years = Array.from(new Set(data.map(d => d.iyear)));

    const yearSelect = d3.select("#yearSelect");
    yearSelect.selectAll("option")
              .data(years)
              .enter()
              .append("option")
              .attr("value", d => d)
              .text(d => d);

    updateChart(years[0]);

    yearSelect.on("change", function() {
        const selectedYear = this.value;
        updateChart(selectedYear);
    });

    function updateChart(year) {
        d3.select("#errorMessage").html("");
        d3.select("#totalIncidents").html("");

        const filteredData = data.filter(d => d.iyear == year);
        const groupCounts = Array.from(d3.group(filteredData, d => d.gname), ([key, value]) => ({
            gname: key,
            incident_count: d3.sum(value, d => +d.incident_count)
        }));

        groupCounts.sort((a, b) => b.incident_count - a.incident_count);
        const topgroups = groupCounts.slice(0, 10);

        if (topgroups.length < 10) {
            d3.select("#errorMessage").html("There are less than 10 Terror groups in the selected year.");
        }

        const totalIncidents = d3.sum(topgroups, d => d.incident_count);
        d3.select("#totalIncidents").html(`Total Incidents in ${year}: ${totalIncidents}`);

        const x = d3.scaleLinear()
                    .domain([0, d3.max(topgroups, d => d.incident_count)])
                    .range([0, chartWidth]);

        const y = d3.scaleBand()
                    .domain(topgroups.map(d => d.gname))
                    .range([0, chartHeight])
                    .padding(0.1);

        svgChart.selectAll("*").remove();

        svgChart.selectAll(".bar")
            .data(topgroups)
            .enter().append("rect")
            .attr("class", "bar")
            .attr("x", 0)
            .attr("y", d => y(d.gname))
            .attr("width", 0)
            .attr("height", y.bandwidth())
            .attr("fill", d => d3.interpolateReds(d.incident_count / d3.max(topgroups, d => d.incident_count)))
            .on("mouseover", (event, d) => {
                tooltip.style("opacity", 1)
                    .html(`<strong>${d.gname}</strong><br>Incidents: ${d.incident_count}`)
                    .style("left", (event.pageX + 10) + "px")
                    .style("top", (event.pageY - 10) + "px");
            })
            .on("mousemove", (event) => {
                tooltip.style("left", (event.pageX + 10) + "px")
                    .style("top", (event.pageY - 10) + "px");
            })
            .on("mouseout", () => tooltip.style("opacity", 0))
            .transition()
            .duration(1000)
            .attr("width", d => x(d.incident_count));

        svgChart.append("g")
           .attr("class", "x-axis")
           .attr("transform", `translate(0,${chartHeight})`)
           .call(d3.axisBottom(x).ticks(5))
           .transition()
           .duration(1000);

        svgChart.append("g")
           .attr("class", "y-axis")
           .call(d3.axisLeft(y))
           .transition()
           .duration(1000);
    }
}).catch(error => console.error(error));

