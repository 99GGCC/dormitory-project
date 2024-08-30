package com.dormitory.controller.web.student;


import com.dormitory.common.R;
import com.dormitory.controller.qry.BedInfoQry;
import com.dormitory.controller.vo.BedInfoVO;
import com.dormitory.service.BedInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

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
@RequestMapping("/student/bed/info")
@Api(value = "StudentBedInfoController", tags = {"床位信息接口"})
public class StudentBedInfoController {

    /**
     * 床位信息Service
     */
    private final BedInfoService bedInfoService;

    @ApiOperation("床位信息列表查询")
    @GetMapping("/list")
    public R<List<BedInfoVO>> listByQry(BedInfoQry qry) {
        return R.success(bedInfoService.listByQry(qry));
    }

}

