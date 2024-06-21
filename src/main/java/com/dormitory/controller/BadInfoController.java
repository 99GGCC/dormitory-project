package com.dormitory.controller;


import io.swagger.annotations.Api;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.RequestMapping;

import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 * 床位信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/bad/info")
@Api(value = "BadInfoController", tags = {"床位信息接口"})
public class BadInfoController {

}

