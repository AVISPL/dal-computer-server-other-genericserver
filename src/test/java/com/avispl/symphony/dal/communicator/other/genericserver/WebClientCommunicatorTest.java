/*
 * Copyright (c) 2015-2021 AVI-SPL, Inc. All Rights Reserved.
 */

package com.avispl.symphony.dal.communicator.other.genericserver;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.Map;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.junit.Rule;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.avispl.symphony.api.dal.dto.monitor.ExtendedStatistics;
import com.avispl.symphony.dal.communicator.HttpCommunicator;

/**
 * Unit test for {@link WebClientCommunicator}.
 * Cover cases: success as 200 in response status code for different content types such as html, xml, jpg; API Error such as 300,
 * 400, 401, 403, 404, 500 in response status code; URI with full path and short path, protocol as http and https
 */
public class WebClientCommunicatorTest {
	private static final int PORT = 8088;
	private static final int HTTPS_PORT = 8443;
	private static final String HOST_NAME = "127.0.0.1";
	private static final String PROTOCOL = "http";
	static WebClientCommunicator webClientCommunicator;

	@Rule
	public WireMockRule wireMockRule = new WireMockRule(options().port(PORT).httpsPort(HTTPS_PORT)
			.bindAddress(HOST_NAME));

	@BeforeEach
	public void init() throws Exception {
		wireMockRule.start();
		webClientCommunicator = new WebClientCommunicator();
		webClientCommunicator.setTrustAllCertificates(true);
		webClientCommunicator.setProtocol(PROTOCOL);
		webClientCommunicator.setPort(PORT);
		webClientCommunicator.setHost(HOST_NAME);
		webClientCommunicator.setContentType("text/plain");
		webClientCommunicator.setAuthenticationScheme(HttpCommunicator.AuthenticationScheme.None);
		webClientCommunicator.init();
	}

	@AfterEach
	public void stopWireMockRule() {
		webClientCommunicator.destroy();
		wireMockRule.stop();
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 200 OK for HTML content
	 */
	@Test
	public void getMultipleStatisticsWithHtmlPage() throws Exception {
		// Attempt to check accessible for index.html
		webClientCommunicator.setURI("/");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 200 OK for XML file
	 */
	@Test
	public void getMultipleStatisticsWithXMLFile() throws Exception {
		// Attempt to check accessible for a xml file
		webClientCommunicator.setURI("/xml-file.xml");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 200 OK for Jpg file
	 */
	@Test
	public void getMultipleStatisticsWithJpgFile() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/jpg-file.jpg");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 403 Forbidden.
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode403() throws Exception {
		//  Expect throw API Error
		webClientCommunicator.setURI("/forbidden");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("403 Forbidden", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with API Error due to 404 response status code.
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode404() {
		//  Expect throw API Error
		webClientCommunicator.setURI("/not-exist-uri");
		try {
			ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
			fail("Expect fail here due to status code 404 out of range 200");
		} catch (Exception e) {
			// expected
			assertEquals("GET operation failed", e.getMessage());
		}
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with default case that does not configure URI.
	 */
	@Test
	public void getMultipleStatisticsWithDefault() throws Exception {
		// should be "Not Configured" when the URI are not set in Adapter Properties
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("Not Configured", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 401 Unauthorized
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode401() throws Exception {
		//  Should be "401 Unauthorized" when the URI are not set in Adapter Properties
		webClientCommunicator.setURI("invalid-login");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("401 Unauthorized", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with API Error due to 500 response status code.
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode500() {
		//  Expect throw API Error
		webClientCommunicator.setURI("internal-server-error");
		try {
			ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
			fail("Expect fail here due to status code 500 out of range 200");
		} catch (Exception e) {
			assertEquals("GET operation failed", e.getMessage());
		}
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with API Error due to 300 response status code.
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode300() throws Exception {
		//  Expect throw API Error
		webClientCommunicator.setURI("/redirect");
		try {
			ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
			fail("Expect fail here due to status code 300 out of range 200");
		} catch (Exception e) {
			assertEquals("GET operation failed", e.getMessage());
		}
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code as 200 and full path URI.
	 */
	@Test
	public void getMultipleStatisticsWithFullPath() throws Exception {
		//  Expect 200 OK for full path of URI
		webClientCommunicator.setURI("http://localhost:8088/");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code as 200 and https protocol being used.
	 */
	@Test
	public void getMultipleStatisticsWithHttpsProtocol() throws Exception {
		//  Expect 200 OK for HTTPS protocol
		webClientCommunicator.destroy();
		webClientCommunicator.setProtocol("https");
		webClientCommunicator.setPort(wireMockRule.httpsPort());
		webClientCommunicator.setContentType("text/plain");
		webClientCommunicator.setTrustAllCertificates(true);
		webClientCommunicator.init();
		webClientCommunicator.setURI("/https");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		System.out.println(stats.get("URI Status"));
		assertEquals("200 OK", stats.get("URI Status"));
	}
}
